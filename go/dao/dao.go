package dao

import (
	"context"
	"database/sql"
	"errors"

	_ "github.com/jackc/pgx/v5/stdlib"
)

type myDb struct {
	db *sql.DB
}

func CreateDao(ctx context.Context, dsn string) (Dao, error) {
	db, err := sql.Open("pgx", dsn)
	if err != nil {
		return nil, err
	}

	_, err = db.ExecContext(ctx, `CREATE TABLE users (
        id SERIAL PRIMARY KEY,
        name TEXT
    )`)
	if err != nil {
		return nil, err
	}

	if err := db.PingContext(ctx); err != nil {
		return nil, err
	}

	return &myDb{db: db}, nil
}

func (db *myDb) Create(ctx context.Context, u *User) (UserID, error) {
	var out UserID
	err := db.db.QueryRowContext(ctx, "INSERT INTO users(name) VALUES($1) RETURNING id", u.Name).Scan(&out)
	if err != nil {
		return -1, err
	}

	if err := db.db.PingContext(ctx); err != nil {
		return -1, err
	}

	return out, nil
}

func (db *myDb) Update(ctx context.Context, u *User) error {
	res, err := db.db.ExecContext(ctx, "UPDATE users SET name = $1 WHERE id = $2", u.Name, u.ID)
	if err != nil {
		return err
	}

	all, err := res.RowsAffected()
	if err != nil {
		return err
	}

	if all == 0 {
		return errors.New("zero changed lines")
	}

	if err = db.db.PingContext(ctx); err != nil {
		return err
	}
	return err
}

func (db *myDb) Delete(ctx context.Context, id UserID) error {
	_, err := db.db.ExecContext(ctx, "DELETE FROM users WHERE id = $1", id)
	if err != nil {
		return err
	}

	if err := db.db.PingContext(ctx); err != nil {
		return err
	}

	return nil
}

func (db *myDb) Lookup(ctx context.Context, id UserID) (User, error) {
	var user User
	result, err := db.db.QueryContext(ctx, "SELECT id, name FROM users WHERE id = $1", id)

	if err != nil {
		return User{}, err
	}
	defer result.Close()

	if !result.Next() {
		return User{}, sql.ErrNoRows
	}

	if err := result.Scan(&user.ID, &user.Name); err != nil {
		return User{}, err
	}
	return user, nil
}

func (db *myDb) List(ctx context.Context) ([]User, error) {
	var users []User
	data, err := db.db.QueryContext(ctx, "SELECT id, name FROM users")

	if err != nil {
		return nil, err
	}
	defer data.Close()

	for data.Next() {
		var user User
		if err := data.Scan(&user.ID, &user.Name); err != nil {
			return nil, err
		}
		users = append(users, user)
	}
	return users, nil
}

func (db *myDb) Close() error {
	return db.db.Close()
}
