package validation

import (
	"database/sql" // Importar sql para sql.ErrNoRows
	"errors"
	"reflect"
	"strings"
	"unicode"

	"github.com/go-playground/validator/v10"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/postgres" // Necessário se StoreValidator usa *postgres.DB
	"go.uber.org/zap"
)

var (
	ErrInvalidContentLength = errors.New("invalid content length")
	ErrInvalidMimeType      = errors.New("invalid mime type")
)

type StoreValidator struct {
	// Mantém a conexão base, mas as verificações que precisam de RLS usarão o Queryable passado.
	db  *postgres.DB
	log *zap.Logger
}

func New(db *postgres.DB, log *zap.Logger) *StoreValidator {
	return &StoreValidator{db: db, log: log}
}

// EmailUnique continua global, usa a conexão base db.
func (sv *StoreValidator) EmailUnique(fl validator.FieldLevel) bool {
	var result bool
	query := `SELECT EXISTS (SELECT id FROM accounts WHERE email = $1)`
	// Usar sv.db.DB.DB para obter *sql.DB se necessário, ou sv.db diretamente se for *sqlx.DB
	// Assumindo que sv.db é *postgres.DB que contém *sqlx.DB
	if err := sv.db.Get(&result, query, fl.Field().String()); err != nil {
		sv.log.Error("Failed to execute validation query", zap.String("query", "email-uniqueness"), zap.Error(err))
		return false // Falha fechada por segurança
	}
	return !result // Retorna true se NÃO existir (é único)
}

// NIFUnique foi removido como método automático de validação de struct.
// Usar IsNIFUniqueInTenant no handler.
/*
func (sv *StoreValidator) NIFUnique(fl validator.FieldLevel) bool { ... }
*/

// IsNIFUniqueInTenant verifica a unicidade do NIF DENTRO do tenant atual (via RLS em 'q').
// Retorna true se for único, false se já existir ou se ocorrer um erro.
// É chamada EXPLICITAMENTE no handler.
func IsNIFUniqueInTenant(q npadmin.Queryable, nif string, log *zap.Logger) bool {
	var exists bool
	// Query opera em 'profiles'. RLS aplicada por 'q' filtra pelo tenant_id.
	query := `SELECT EXISTS (SELECT 1 FROM profiles WHERE nif = $1)`
	err := q.Get(&exists, query, nif)

	// Tratar erros
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		// Logar o erro - sql.ErrNoRows não devia acontecer com EXISTS, mas outros erros sim.
		log.Error("Failed to execute NIF uniqueness check query", zap.String("nif", nif), zap.Error(err))
		return false // Falha fechada por segurança
	}

	// Se 'exists' for true, significa que o NIF já existe neste tenant, logo NÃO é único.
	return !exists // Retorna true se NÃO existir (é único)
}

// IsTLMUniqueInTenant verifica a unicidade do TLM DENTRO do tenant atual (via RLS em 'q').
// Retorna true se for único, false se já existir ou se ocorrer um erro.
// É chamada EXPLICITAMENTE no handler.
func IsTLMUniqueInTenant(q npadmin.Queryable, tlm string, log *zap.Logger) bool {
	// TLM pode ser opcional, só verificar se for fornecido
	if tlm == "" {
		return true // Vazio é considerado "único" ou não aplicável
	}

	var exists bool
	// Query opera em 'profiles'. RLS aplicada por 'q' filtra pelo tenant_id.
	query := `SELECT EXISTS (SELECT 1 FROM profiles WHERE tlm = $1)`
	err := q.Get(&exists, query, tlm)

	// Tratar erros
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		log.Error("Failed to execute TLM uniqueness check query", zap.String("tlm", tlm), zap.Error(err))
		return false // Falha fechada por segurança
	}

	// Se 'exists' for true, significa que o TLM já existe neste tenant, logo NÃO é único.
	return !exists // Retorna true se NÃO existir (é único)
}


// Funções de validação de formato mantêm-se iguais
func Acceptance(fl validator.FieldLevel) bool { return fl.Field().Bool() }
func Password(field reflect.Value) any { if value, ok := field.Interface().(crypto.Password); ok { return value.Plaintext }; return nil }
func NullString(field reflect.Value) any { if value, ok := field.Interface().(npadmin.NullString); ok { return value.String }; return nil }
func HasSymbols(fl validator.FieldLevel) bool { fnSymbol := unicode.IsSymbol; fnPunct := unicode.IsPunct; return strings.IndexFunc(fl.Field().String(), fnSymbol) != -1 || strings.IndexFunc(fl.Field().String(), fnPunct) != -1 }
func HasNumbers(fl validator.FieldLevel) bool { fn := unicode.IsDigit; return strings.IndexFunc(fl.Field().String(), fn) != -1 }