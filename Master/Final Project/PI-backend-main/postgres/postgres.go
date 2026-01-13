package postgres

import (
	"log"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/jmoiron/sqlx"
)

type Config struct {
	Dsn          string
	MaxOpenConns int
	MaxIdleConns int
	MaxIdleTime  string
}

type DB struct {
	*sqlx.DB
}

func Connect(cfg Config) *DB {
	dbx, err := sqlx.Connect("postgres", cfg.Dsn)

	if err != nil {
		log.Fatalf("Database connection failed: %v", err)
	}

	db := &DB{DB: dbx}

	duration, err := time.ParseDuration(cfg.MaxIdleTime)
	if err != nil {
		log.Fatalf("Failed to parse duration: %v", err)
	}

	db.SetMaxOpenConns(cfg.MaxOpenConns)
	db.SetMaxIdleConns(cfg.MaxIdleConns)
	db.SetConnMaxIdleTime(duration)

	return db
}

func Transaction(db *DB, fn func(npadmin.Queryable) error) (err error) {
	tx, err := db.Beginx()
	if err != nil {
		return
	}

	defer func() {
		p := recover()

		switch {
		case p != nil:
			_ = tx.Rollback()
			panic(p)
		case err != nil:
			_ = tx.Rollback()
		default:
			err = tx.Commit()
		}
	}()

	err = fn(tx)

	return err
}
