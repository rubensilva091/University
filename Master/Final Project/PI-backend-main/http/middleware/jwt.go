package middleware

import (
	"context"
	"errors"
	"net/http"

	"github.com/invisiblelab-dev/npadmin/crypto"
)

// Use the common ctxKey type declared in other middleware files (do not redeclare it here)
const (
	CtxKeyJwtToken  ctxKey = "jwt_token"
	CtxKeyJwtError  ctxKey = "jwt_error"
	CtxKeyJwtClaims ctxKey = "jwt_claims"
	CtxKeyAccount   ctxKey = "account"
)

var ErrNoJwtTokenFound = errors.New("failed to parse authorization header")

// JwtVerifier accepts one or more token lookup functions and returns a middleware
// that stores the found token (or error) in the request context.
func JwtVerifier(tokenLookupFns ...func(*http.Request) (string, error)) func(http.Handler) http.Handler {
	return verify(tokenLookupFns)
}

// JwtAuthorize validates the token (using crypto.GetJwtClaims) and stores claims in context.
// 'secretOrPath' is forwarded to your crypto.GetJwtClaims implementation (match your codebase).
func JwtAuthorize(secretOrPath string) func(http.Handler) http.Handler {
	return authorize(secretOrPath)
}

func authorize(path string) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			token, _ := r.Context().Value(CtxKeyJwtToken).(string)
			err, _ := r.Context().Value(CtxKeyJwtError).(error)

			if err != nil || token == "" {
				http.Error(w, http.StatusText(http.StatusUnauthorized), http.StatusUnauthorized)
				return
			}

			claims, err := crypto.GetJwtClaims(path, token)
			if err != nil {
				http.Error(w, http.StatusText(http.StatusUnauthorized), http.StatusUnauthorized)
				return
			}

			ctx := context.WithValue(r.Context(), CtxKeyJwtClaims, claims)
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}

func verify(tokenLookupFns []func(*http.Request) (string, error)) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			token, err := lookup(r, tokenLookupFns...)
			ctx := r.Context()
			ctx = context.WithValue(ctx, CtxKeyJwtToken, token)
			ctx = context.WithValue(ctx, CtxKeyJwtError, err)
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}

func lookup(r *http.Request, tokenLookupFns ...func(r *http.Request) (string, error)) (string, error) {
	// Try each lookup function in order, return first successful token
	for _, fn := range tokenLookupFns {
		tok, err := fn(r)
		if err == nil && tok != "" {
			return tok, nil
		}
	}
	return "", ErrNoJwtTokenFound
}
