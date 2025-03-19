//go:build !solution

package ledger

import (
	"context"
	"database/sql"
	"errors"

	_ "github.com/jackc/pgx/v5/stdlib"
)

var ErrNegativeAmount = errors.New("negative amount is not allowed")

type myDb struct {
	db *sql.DB
}

func New(ctx context.Context, dsn string) (Ledger, error) {
	db, err := sql.Open("pgx", dsn)
	if err != nil {
		return nil, err
	}

	_, err = db.ExecContext(ctx, `CREATE TABLE users (
        id TEXT PRIMARY KEY,
        money BIGINT
    )`)
	if err != nil {
		return nil, err
	}

	if err := db.PingContext(ctx); err != nil {
		return nil, err
	}

	return &myDb{db: db}, nil
}

func (db *myDb) CreateAccount(ctx context.Context, id ID) error {
	_, err := db.db.ExecContext(ctx,
		"INSERT INTO users (id, money) VALUES ($1, $2)",
		id, 0)
	if err != nil {
		return err
	}

	if err := db.db.PingContext(ctx); err != nil {
		return err
	}
	return nil
}

func (db *myDb) GetBalance(ctx context.Context, id ID) (Money, error) {
	var money Money
	err := db.db.QueryRowContext(ctx, "SELECT money FROM users WHERE id = $1", id).Scan(&money)
	if err != nil {
		return -1, err
	}

	if money < 0 {
		return -1, ErrNegativeAmount
	}

	if err := db.db.PingContext(ctx); err != nil {
		return -1, err
	}
	return money, nil
}

func (db *myDb) Deposit(ctx context.Context, id ID, amount Money) error {
	tx, err := db.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer func() {
		if newErr := tx.Rollback(); newErr != nil {
			err = newErr
		}
	}()

	if amount < 0 {
		return ErrNegativeAmount
	}

	var curMoney Money
	err = tx.QueryRowContext(ctx, "SELECT money FROM users WHERE id = $1 FOR UPDATE", id).Scan(&curMoney)
	if err != nil {
		return err
	}

	_, err = tx.ExecContext(ctx, "UPDATE users SET money = $1 WHERE id = $2", curMoney+amount, id)

	if err != nil {
		return err
	}

	if err = tx.Commit(); err != nil {
		return err
	}

	if err := db.db.PingContext(ctx); err != nil {
		return err
	}
	return nil
}

func (db *myDb) Withdraw(ctx context.Context, id ID, amount Money) error {
	if amount <= 0 {
		return ErrNegativeAmount
	}

	tx, err := db.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer func() {
		if newErr := tx.Rollback(); newErr != nil {
			err = newErr
		}
	}()

	var money Money
	err = tx.QueryRowContext(ctx, "UPDATE users SET money = money - $1 WHERE id = $2 RETURNING money", amount, id).Scan(&money)
	if err != nil {
		return err
	}

	if money < 0 {
		return ErrNoMoney
	}

	if err = tx.Commit(); err != nil {
		return err
	}

	return nil
}

func (db *myDb) Transfer(ctx context.Context, from, to ID, amount Money) error {
	if amount <= 0 {
		return ErrNegativeAmount
	}

	var first, second ID
	if from < to {
		first = from
		second = to
	} else {
		first = to
		second = from
	}

	tx, err := db.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer func() {
		if newErr := tx.Rollback(); newErr != nil {
			err = newErr
		}
	}()

	var one Money
	var two Money
	err = tx.QueryRowContext(ctx, "SELECT money FROM users WHERE id = $1 FOR UPDATE", first).Scan(&one)
	if err != nil {
		return err
	}

	err = tx.QueryRowContext(ctx, "SELECT money FROM users WHERE id = $1 FOR UPDATE", second).Scan(&two)
	if err != nil {
		return err
	}

	if first == from {
		one -= amount
		two += amount
	} else {
		two -= amount
		one += amount
	}

	if one < 0 || two < 0 {
		return ErrNoMoney
	}

	_, err = tx.ExecContext(ctx, "UPDATE users SET money = $1 WHERE id = $2", one, first)
	if err != nil {
		return err
	}

	_, err = tx.ExecContext(ctx, "UPDATE users SET money = $1 WHERE id = $2", two, second)
	if err != nil {
		return err
	}

	if err = tx.Commit(); err != nil {
		return err
	}

	return nil
}

func (db *myDb) Close() error {
	return db.db.Close()
}
