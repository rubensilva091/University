package npadmin

import (
	"database/sql"
	"encoding/json"
	"fmt"

	"github.com/jmoiron/sqlx"
)

var (
	//nolint
	Version string
	//nolint
	Commit string
)

type ValitationError struct {
	Field      string
	Validation string
}

type Dbx interface {
	Queryable
	Begin() (Tx, error)
	Beginx() (Tx, error)
}

type Tx interface {
	Queryable
	Commit() error
	Rollback() error
}

type Queryable interface {
	Get(dest interface{}, query string, args ...interface{}) error
	QueryRow(query string, args ...any) *sql.Row
	Select(dest interface{}, query string, args ...interface{}) error
	NamedQuery(query string, arg interface{}) (*sqlx.Rows, error)
	NamedExec(query string, arg interface{}) (sql.Result, error)
	Exec(query string, args ...any) (sql.Result, error)
	PrepareNamed(query string) (*sqlx.NamedStmt, error)
}

type Transactional struct {
	Db Queryable
}

type PushNotification struct {
	Channel string
	Data    map[string]string
}

func WithTx(tx Queryable) func(*Transactional) {
	return func(t *Transactional) {
		t.Db = tx
	}
}

func GetQueriable(db Queryable, opts ...func(*Transactional)) Queryable {
	q := Transactional{Db: db}
	for _, opt := range opts {
		opt(&q)
	}

	return q.Db
}

// Override sql.NullString
type NullString struct {
	sql.NullString
}

func NewNullString(value string) NullString {
	ns := NullString{}
	ns.String = value
	ns.Valid = true

	return ns
}

func (ns NullString) MarshalJSON() ([]byte, error) {
	if ns.Valid {
		b, err := json.Marshal(ns.String)
		if err != nil {
			return nil, fmt.Errorf("failed to marshal NullString: %w", err)
		}

		return b, nil
	}

	b, err := json.Marshal(nil)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal NullString nil: %w", err)
	}

	return b, nil
}

func (ns *NullString) UnmarshalJSON(data []byte) error {
	var value *string
	if err := json.Unmarshal(data, &value); err != nil {
		return fmt.Errorf("failed to unmarshal NullString: %w", err)
	}

	if value != nil {
		ns.Valid = true
		ns.String = *value
	} else {
		ns.Valid = false
	}

	return nil
}

type NullInt64 struct {
	sql.NullInt64
}

func NewNullInt64(value int64) NullInt64 {
	ns := NullInt64{}
	ns.Int64 = value
	ns.Valid = true

	return ns
}

func (ns NullInt64) MarshalJSON() ([]byte, error) {
	if ns.Valid {
		b, err := json.Marshal(ns.Int64)
		if err != nil {
			return nil, fmt.Errorf("failed to marshal NullInt64: %w", err)
		}

		return b, nil
	}

	b, err := json.Marshal(nil)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal NullInt64 nil: %w", err)
	}

	return b, nil
}

func (ns *NullInt64) UnmarshalJSON(data []byte) error {
	var value *int64
	if err := json.Unmarshal(data, &value); err != nil {
		return fmt.Errorf("failed to unmarshal NullInt64: %w", err)
	}

	if value != nil {
		ns.Valid = true
		ns.Int64 = *value
	} else {
		ns.Valid = false
	}

	return nil
}
