// Package crypto provides helpers for tokens and hashing.
package crypto

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/base32"
	"encoding/hex"
	"fmt"
	"time"

	"github.com/segmentio/ksuid"
)

// VerificationTokenLen is the byte length used to generate verification tokens.
const (
	VerificationTokenLen = 32
)

// Token represents a verification token with its plaintext and SHA-256 hash.
type Token struct {
	Plaintext string
	Hash      []byte
	Deadline  time.Time
}

// GenerateVerificationToken creates a new verification token valid for the given ttl.
func GenerateVerificationToken(ttl time.Duration) (*Token, error) {
	token := &Token{
		Deadline: time.Now().UTC().Add(ttl),
	}

	//nolint:mnd // 20 bytes for verification token
	buffer := make([]byte, 20)

	if _, err := rand.Read(buffer); err != nil {
		return nil, fmt.Errorf("failed to generate verification token: %w", err)
	}

	token.Plaintext = base32.StdEncoding.EncodeToString(buffer)
	hash := sha256.Sum256([]byte(token.Plaintext))
	token.Hash = hash[:]

	return token, nil
}

// GenerateString returns a random 16-byte hex string.
func GenerateString() string {
	//nolint:mnd // 16 bytes for random string generation
	buffer := make([]byte, 16)
	_, _ = rand.Read(buffer)

	return hex.EncodeToString(buffer)
}

// Sha256Checksum returns the hex-encoded SHA-256 checksum of the given string.
func Sha256Checksum(plain string) string {
	hash := sha256.Sum256([]byte(plain))

	return hex.EncodeToString(hash[:])
}

// MatchVerificationToken checks whether a plaintext token matches a given hex-encoded SHA-256 hash.
func MatchVerificationToken(plain string, hash string) bool {
	inputHash := sha256.Sum256([]byte(plain))

	return hex.EncodeToString(inputHash[:]) == hash
}

// HexEncode returns a hex-encoded string of the provided bytes.
func HexEncode(bytes []byte) string {

	return hex.EncodeToString(bytes)
}

// GetRandomID returns a globally unique, k-sortable identifier.
func GetRandomID() string {
	return ksuid.New().String()
}
