package middleware

import (
	"context"
	"database/sql" // Importar sql para sql.ErrNoRows
	"errors"       // Importar errors
	"net/http"
	"fmt"
	"strconv"


	"github.com/golang-jwt/jwt/v4"
	"github.com/invisiblelab-dev/npadmin"
)

// AccountCtx obtém a conta global baseada no JWT e injeta-a no contexto,
// juntamente com o ID numérico do utilizador.
// DEVE ser executado DEPOIS de JwtVerifier, JwtAuthorize, e TenantTxMiddleware.
func AccountCtx(as npadmin.AccountService) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		fn := func(w http.ResponseWriter, r *http.Request) {
			// 1. Obter Claims JWT
			claims, ok := r.Context().Value(CtxKeyJwtClaims).(*jwt.RegisteredClaims)
			if !ok || claims == nil || claims.Subject == "" {
				// Se não houver claims válidos, não podemos identificar o utilizador.
				// Para rotas protegidas, isto não devia acontecer (JwtAuthorize falharia antes).
				// Se for uma rota opcionalmente autenticada, podemos prosseguir sem conta no contexto.
				// --> Por agora, assumimos que as rotas que usam AccountCtx REQUEREM autenticação.
				http.Error(w, http.StatusText(http.StatusUnauthorized)+": Missing or invalid JWT claims", http.StatusUnauthorized)
				return
			}

			// 2. Extrair Queryable (transação 'tx') do contexto
			tx, ok := r.Context().Value(CtxKeyTx).(npadmin.Queryable)
			if !ok {
				// Isto não deve acontecer se TenantTxMiddleware correu antes.
				// Logar erro interno.
				// s.LogError(r, errors.New("database transaction missing in AccountCtx")) // Precisaria do logger aqui
				fmt.Println("ERRO INTERNO: Transação DB em falta no AccountCtx") // Log simples
				http.Error(w, "Internal server error: DB context missing", http.StatusInternalServerError)
				return
			}

			// 3. Buscar a Conta Global usando o email (Subject) e a transação 'tx'
			// O AccountService.GetByEmail agora busca na tabela global
			account, err := as.GetByEmail(tx, claims.Subject)
			if err != nil {
				// Tratar erro (ex: utilizador no JWT não existe mais na BD?)
				if errors.Is(err, sql.ErrNoRows) {
					http.Error(w, http.StatusText(http.StatusUnauthorized)+": User not found", http.StatusUnauthorized)
				} else {
					// Logar outro erro de BD
					// s.LogError(r, fmt.Errorf("failed to get account by email in AccountCtx: %w", err))
					fmt.Printf("ERRO DB no AccountCtx ao buscar conta: %v\n", err) // Log simples
					http.Error(w, "Internal server error retrieving user", http.StatusInternalServerError)
				}
				return
			}

			// 4. Injetar a conta e o ID no contexto
			ctx := r.Context()
			ctx = context.WithValue(ctx, CtxKeyAccount, account)    // A struct Account completa
			ctx = context.WithValue(ctx, CtxKeyUserID, account.ID) // O ID numérico (int64)

			// Chamar próximo handler
			next.ServeHTTP(w, r.WithContext(ctx))
		}

		return http.HandlerFunc(fn)
	}
}

// AdminCtx verifica se o utilizador autenticado tem o papel 'admin' NO TENANT ATUAL.
// DEVE ser executado DEPOIS de JwtVerifier, JwtAuthorize, TenantTxMiddleware, e AccountCtx.
func AdminCtx(as npadmin.AccountService, ps npadmin.ProfileService) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		fn := func(w http.ResponseWriter, r *http.Request) {
			// 1. Obter a conta global e ID do contexto (definidos por AccountCtx)
			account, okAcc := r.Context().Value(CtxKeyAccount).(*npadmin.Account)
			accountID, okID := r.Context().Value(CtxKeyUserID).(int64)

			if !okAcc || account == nil || !okID || accountID == 0 {
				// AccountCtx deve ter corrido antes e encontrado a conta. Se não, erro.
				// s.LogError(r, errors.New("account or user ID missing from context in AdminCtx"))
				fmt.Println("ERRO INTERNO: Contexto de conta/utilizador em falta no AdminCtx") // Log simples
				http.Error(w, "Internal server error: User context missing", http.StatusInternalServerError)
				return
			}

			// 2. Extrair Queryable (transação 'tx') do contexto (inclui tenant_id e user_id para RLS)
			tx, ok := r.Context().Value(CtxKeyTx).(npadmin.Queryable)
			if !ok {
				// s.LogError(r, errors.New("database transaction missing in AdminCtx"))
				fmt.Println("ERRO INTERNO: Transação DB em falta no AdminCtx") // Log simples
				http.Error(w, "Internal server error: DB context missing", http.StatusInternalServerError)
				return
			}

			// 3. Extrair tenantID do contexto (string, definido por TenantTxMiddleware)
			tenantIDStr, okTenant := r.Context().Value(CtxKeyTenantID).(string)
			if !okTenant || tenantIDStr == "" {
				// TenantTxMiddleware deve ter definido isto.
				// s.LogError(r, errors.New("tenant ID missing from context in AdminCtx"))
				fmt.Println("ERRO INTERNO: Tenant ID em falta no contextp AdminCtx") // Log simples
				http.Error(w, "Internal server error: Tenant context missing", http.StatusInternalServerError)
				return
			}
			tenantID, _ := strconv.ParseInt(tenantIDStr, 10, 64) // Ignorar erro, já validado em TenantTxMiddleware

			// 4. Verificar o papel na tabela de memberships PARA o tenant atual
			var role string
			query := `SELECT role FROM account_tenant_memberships
			          WHERE account_id = $1 AND tenant_id = $2`
			err := tx.Get(&role, query, accountID, tenantID)

			if err != nil {
				if errors.Is(err, sql.ErrNoRows) {
					// Utilizador autenticado globalmente, mas não é membro deste tenant específico.
					http.Error(w, http.StatusText(http.StatusForbidden)+": Not a member of this tenant", http.StatusForbidden)
				} else {
					// Outro erro de BD
					// s.LogError(r, fmt.Errorf("failed to get membership role in AdminCtx: %w", err))
					fmt.Printf("ERRO DB no AdminCtx ao buscar role: %v\n", err) // Log simples
					http.Error(w, "Internal server error checking permissions", http.StatusInternalServerError)
				}
				return
			}

			// 5. Verificar se o papel é 'admin'
			if role != "admin" {
				http.Error(w, http.StatusText(http.StatusForbidden)+": Admin role required", http.StatusForbidden)
				return
			}

			// 6. Utilizador é admin neste tenant, prosseguir
			// O contexto já tem CtxKeyAccount e CtxKeyUserID definidos por AccountCtx
			next.ServeHTTP(w, r)
		}
		return http.HandlerFunc(fn)
	}
}