package crypto

import (
	"crypto/rand"
	"errors"
	"fmt"
	"strconv"

	"golang.org/x/crypto/bcrypt"
)

var ErrInvalidPasswordFormat = errors.New("invalid password format")

type Password struct {
	Plaintext string
	Hash      []byte
}

const (
	numbers = "0123456789"
	symbols = "!@#$%&*+_-="
	chars   = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%&*+_-="
)

func (p *Password) Set(password string) error {
	//nolint:mnd // bcrypt cost factor 14 is a security parameter
	hash, err := bcrypt.GenerateFromPassword([]byte(password), 14)
	if err != nil {
		return fmt.Errorf("failed to generate password: %w", err)
	}

	p.Plaintext = password
	p.Hash = hash

	return nil
}

func (p *Password) Matches() (bool, error) {
	if err := bcrypt.CompareHashAndPassword(p.Hash, []byte(p.Plaintext)); err != nil {
		return false, fmt.Errorf("passwords do not match: %w", err)
	}

	return true, nil
}

func (p *Password) UnmarshalJSON(value []byte) error {
	unquotedValue, err := strconv.Unquote(string(value))
	if err != nil {
		return ErrInvalidPasswordFormat
	}

	return p.Set(unquotedValue)
}

// TODO: review password generator
func RandomPassword() (Password, error) {
	//nolint:mnd // 10 characters per segment for password generation
	fullHash, err := hasher(10, chars)
	if err != nil {
		return Password{}, err
	}

	//nolint:mnd // 10 characters per segment for password generation
	symbolHash, err := hasher(10, symbols)
	if err != nil {
		return Password{}, err
	}

	//nolint:mnd // 10 characters per segment for password generation
	numberHash, err := hasher(10, numbers)
	if err != nil {
		return Password{}, err
	}

	fullHash = append(fullHash, symbolHash...)
	fullHash = append(fullHash, numberHash...)

	return Password{Hash: fullHash, Plaintext: string(fullHash)}, nil
}

func hasher(long int, str string) ([]byte, error) {
	blk := make([]byte, long)
	if _, err := rand.Read(blk); err != nil {
        return nil, fmt.Errorf("failed to read random bytes: %w", err)
	}

	for i, b := range blk {
		blk[i] = str[b%byte(len(str))]
	}

	return blk, nil
}
