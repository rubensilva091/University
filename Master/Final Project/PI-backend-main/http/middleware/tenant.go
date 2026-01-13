package middleware

import (
	"context"
	"database/sql"
	"fmt"    // Importar fmt
	"net/http"
	"strconv"

	"github.com/golang-jwt/jwt/v4" // IMPORTAR jwt/v4
	"github.com/jmoiron/sqlx"      // IMPORT SQLX
)

// Definir chaves de contexto (manter consistência)
type ctxKey string

const CtxKeyTx ctxKey = "db_tx"
const CtxKeyTenantID ctxKey = "tenant_id"
const CtxKeyUserID ctxKey = "user_id" // Nova chave para o ID do utilizador global

func TenantTxMiddleware(db *sql.DB) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// --- 1. Determinar Tenant ID ---
			var tenantID string

			if v := r.Context().Value(CtxKeyTenantID); v != nil {
				switch t := v.(type) {
				case string:
					tenantID = t
				case int64:
					tenantID = strconv.FormatInt(t, 10)
				case int:
					tenantID = strconv.Itoa(t)
				}
			}

			if tenantID == "" {
				tenantID = r.Header.Get("X-Tenant-ID")
			}

			if tenantID == "" {
				http.Error(w, "tenant id missing in request context or X-Tenant-ID header", http.StatusBadRequest)
				return
			}
			if _, err := strconv.ParseInt(tenantID, 10, 64); err != nil {
				http.Error(w, "invalid tenant id format", http.StatusBadRequest)
				return
			}

			// --- 2. Determinar User ID (CORREÇÃO APLICADA AQUI) ---
			dbx := sqlx.NewDb(db, "postgres")
			var userID string = "0"
			var accountID int64 = 0
			var emailSubject string

			// Tenta obter claims do contexto
			rawClaims := r.Context().Value(CtxKeyJwtClaims)
			
			if rawClaims != nil {
				// TENTATIVA A: Tipo RegisteredClaims (Struct específica)
				if claims, ok := rawClaims.(*jwt.RegisteredClaims); ok {
					emailSubject = claims.Subject
				} else if claims, ok := rawClaims.(jwt.MapClaims); ok {
					// TENTATIVA B: Tipo MapClaims (Map genérico - O MAIS COMUM)
					// O campo 'sub' guarda o email/ID do utilizador
					if sub, okSub := claims["sub"].(string); okSub {
						emailSubject = sub
					}
				}
			}

			if emailSubject != "" {
				// Buscar o ID da conta pelo email
				err := dbx.Get(&accountID, "SELECT id FROM accounts WHERE email = $1", emailSubject)
				if err != nil {
					fmt.Printf("Middleware: User %s não encontrado na BD: %v\n", emailSubject, err)
					accountID = 0
					userID = "0"
				} else {
					userID = strconv.FormatInt(accountID, 10)
					// Log de confirmação (podes remover depois)
					fmt.Printf("Middleware SUCESSO: User %s (ID %s) autenticado no Tenant %s\n", emailSubject, userID, tenantID)
				}
			} else {
				fmt.Println("Middleware AVISO: Token válido mas não foi possível ler o 'sub' (email).")
			}

			// --- 3. Iniciar Transação e Definir RLS ---
			tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
			if err != nil {
				fmt.Printf("Erro DB Begin: %v\n", err)
				http.Error(w, "database error", http.StatusInternalServerError)
				return
			}
			committed := false
			defer func() {
				if !committed {
					_ = tx.Rollback()
				}
			}()

			// Definir tenant_id
			if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.tenant_id', $1, true)", tenantID); err != nil {
				fmt.Printf("Erro RLS Tenant: %v\n", err)
				_ = tx.Rollback()
				http.Error(w, "failed to set tenant context", http.StatusInternalServerError)
				return
			}

			// Definir user_id (CRUCIAL para is_session_admin funcionar)
			if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.user_id', $1, true)", userID); err != nil {
				fmt.Printf("Erro RLS User: %v\n", err)
				_ = tx.Rollback()
				http.Error(w, "failed to set user context", http.StatusInternalServerError)
				return
			}

			// --- 4. Injetar no Contexto ---
			ctx := context.WithValue(r.Context(), CtxKeyTx, tx)
			ctx = context.WithValue(ctx, CtxKeyTenantID, tenantID)
			ctx = context.WithValue(ctx, CtxKeyUserID, accountID)

			next.ServeHTTP(w, r.WithContext(ctx))

			// --- 5. Commit ---
			if err := tx.Commit(); err != nil {
				fmt.Printf("Erro DB Commit: %v\n", err)
				return
			}
			committed = true
		})
	}
}

// UserTxMiddleware (NOVO) é um middleware para iniciar uma transação
// e definir APENAS o 'myapp.user_id' para RLS, sem exigir um 'myapp.tenant_id'.
func UserTxMiddleware(db *sql.DB) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// 1. Conectar ao DB
			dbx := sqlx.NewDb(db, "postgres")

			// 2. Determinar User ID (do JWT)
			var userID string = "0"
			var accountID int64 = 0

			// Obter claims do contexto (definido pelo JwtAuthorize middleware)
			claims, ok := r.Context().Value(CtxKeyJwtClaims).(*jwt.RegisteredClaims)
			if ok && claims != nil && claims.Subject != "" {
				// Buscar o ID da conta pelo email (Subject)
				err := dbx.Get(&accountID, "SELECT id FROM accounts WHERE email = $1", claims.Subject)
				if err != nil {
					fmt.Printf("Aviso: Utilizador do JWT (email: %s) não encontrado na BD em UserTxMiddleware: %v\n", claims.Subject, err)
					accountID = 0
					userID = "0"
				} else {
					userID = strconv.FormatInt(accountID, 10)
				}
			} else {
				// Se não houver claims (ou for inválido), este middleware deve falhar?
				// Assumindo que as rotas que usam isto SÃO protegidas, JwtAuthorize já falhou.
				// Mas por segurança, se chegarmos aqui sem ID, é um erro.
				http.Error(w, "UserTxMiddleware: user claims missing or invalid", http.StatusUnauthorized)
				return
			}
			
			// Se accountID for 0, o utilizador não foi encontrado.
			if accountID == 0 {
				http.Error(w, "UserTxMiddleware: user not found", http.StatusUnauthorized)
				return
			}

			// 3. Iniciar Transação
			tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
			if err != nil {
				fmt.Printf("Erro ao iniciar transação em UserTxMiddleware: %v\n", err)
				http.Error(w, "database error", http.StatusInternalServerError)
				return
			}
			committed := false
			defer func() {
				if !committed {
					_ = tx.Rollback()
				}
			}()

			// 4. Definir user_id para RLS (mas NÃO tenant_id)
			if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.user_id', $1, true)", userID); err != nil {
				fmt.Printf("Erro ao definir myapp.user_id em UserTxMiddleware: %v\n", err)
				_ = tx.Rollback()
				http.Error(w, "failed to set user context", http.StatusInternalServerError)
				return
			}
			
			// --- INÍCIO DA CORREÇÃO ---
			// Definir um tenant_id placeholder ('0') para que a RLS
			// (tenant_id = X::bigint OR user_id = Y::bigint)
			// não falhe na conversão de tipo de X.
			if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.tenant_id', '0', true)"); err != nil {
				fmt.Printf("Erro ao definir myapp.tenant_id=0 em UserTxMiddleware: %v\n", err)
				_ = tx.Rollback()
				http.Error(w, "failed to set tenant context", http.StatusInternalServerError)
				return
			}
			// --- FIM DA CORREÇÃO ---


			// 5. Injetar no Contexto
			ctx := context.WithValue(r.Context(), CtxKeyTx, tx)    // tx é *sqlx.Tx
			ctx = context.WithValue(ctx, CtxKeyUserID, accountID) // int64 - para uso fácil
			// NÃO definimos CtxKeyTenantID

			// Chamar próximo handler
			next.ServeHTTP(w, r.WithContext(ctx))

			// 6. Commit
			if err := tx.Commit(); err != nil {
				fmt.Printf("Erro ao commitar transação em UserTxMiddleware: %v\n", err)
				return
			}
			committed = true
		})
	}
}