package crypto

import (
	"errors"
	"fmt"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/golang-jwt/jwt/v4"
	"github.com/google/uuid"
)

var (
	ErrNoAuthorizationHeader   = errors.New("failed to find authorization header")
	ErrNoJwtTokenFound         = errors.New("failed to parse authorization header")
	ErrInvalidJwtSigningMethod = errors.New("invalid JWT signing method")
	ErrInvalidToken            = errors.New("invalid JWT token failed to parse claims")
)

// NewJwtToken generates a new JWT token using only standard claims.
func NewJwtToken(username string, path string, ttl time.Duration) (string, error) {
	return NewJwtTokenWithClaims(username, path, ttl, nil)
}

// NewJwtTokenWithClaims generates a new JWT token with standard and custom claims.
func NewJwtTokenWithClaims(username string, path string, ttl time.Duration, customClaims map[string]interface{}) (string, error) {
	key, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("failed to read jwt private key (%s): %w", path, err)
	}

	parsedKey, err := jwt.ParseEdPrivateKeyFromPEM(key)
	if err != nil {
		return "", fmt.Errorf("failed to parse jwt private key: %w", err)
	}

	deadline := jwt.NewNumericDate(time.Now().UTC().Add(ttl))
	now := time.Now().UTC()

	// Use MapClaims to hold both registered and custom claims
	claims := jwt.MapClaims{
		"exp": deadline.Unix(),
		"sub": username,
		"iat": now.Unix(),
		"nbf": now.Unix(),
		"iss": "TODO: resolve issuing FQDN",
		"jti": uuid.NewString(),
	}

	// Merge custom claims
	for k, v := range customClaims {
		claims[k] = v
	}
	
	// Ensure subject is always the username/email
	claims["sub"] = username

	token := jwt.NewWithClaims(jwt.SigningMethodEdDSA, claims)
	tokenString, err := token.SignedString(parsedKey)
	if err != nil {
		return "", fmt.Errorf("failed to sign jwt token: %w", err)
	}

	return tokenString, nil
}


func GetJwtClaims(path string, payload string) (*jwt.RegisteredClaims, error) {
	keyFunc := func(token *jwt.Token) (any, error) {
		if _, ok := token.Method.(*jwt.SigningMethodEd25519); !ok {
			return nil, ErrInvalidJwtSigningMethod
		}

		key, err := os.ReadFile(path)
		if err != nil {
			return nil, fmt.Errorf("failed to read jwt public key (%s): %w", path, err)
		}

		parsedKey, err := jwt.ParseEdPublicKeyFromPEM(key)
		if err != nil {
			return nil, fmt.Errorf("failed to parse jwt public key: %w", err)
		}

		return parsedKey, nil
	}

	// Change to MapClaims for parsing to ensure custom claims are read
	token, err := jwt.ParseWithClaims(payload, jwt.MapClaims{}, keyFunc)
	if err != nil {
		return nil, fmt.Errorf("failed to parse jwt claims: %w", err)
	}

	claims, ok := token.Claims.(jwt.MapClaims)
	if !ok {
		return nil, ErrInvalidToken
	}

	// Manually map registered claims back for the return signature
	rc := &jwt.RegisteredClaims{}
	if sub, ok := claims["sub"].(string); ok {
		rc.Subject = sub
	}
	if id, ok := claims["jti"].(string); ok {
		rc.ID = id
	}
	if exp, ok := claims["exp"].(float64); ok {
		rc.ExpiresAt = jwt.NewNumericDate(time.Unix(int64(exp), 0))
	} else if exp, ok := claims["exp"].(int64); ok {
		rc.ExpiresAt = jwt.NewNumericDate(time.Unix(exp, 0))
	}
	
	return rc, nil
}

func ParseAuthorizationHeader(r *http.Request) (string, error) {
	payload := r.Header.Get("Authorization")
	if payload == "" {
		return "", ErrNoAuthorizationHeader
	}

	tokens := strings.Split(payload, " ")
	//nolint:mnd // Authorization header format is "Bearer <token>"
	if len(tokens) < 2 {
		return "", ErrNoJwtTokenFound
	}

	return tokens[1], nil
}

// Authenticate is a helper function to check for authentication header on endpoints
// that accept unauthenticated and authenticated request.
func Authenticate(r *http.Request, path string) (*jwt.RegisteredClaims, error) {
	token, err := ParseAuthorizationHeader(r)
	if err != nil {
		return nil, fmt.Errorf("failed to parse authorization headers during authentication: %w", err)
	}

	claims, err := GetJwtClaims(path, token)
	if err != nil {
		return nil, fmt.Errorf("failed to get claims during authentication: %w", err)
	}

	return claims, nil
}