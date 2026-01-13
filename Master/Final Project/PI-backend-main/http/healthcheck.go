package http

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/invisiblelab-dev/npadmin"
)

func (s *Server) registerHealthCheckRoutes(r chi.Router) {
	r.Get("/healthcheck", s.handleHealthCheck)
}

func (s *Server) handleHealthCheck(w http.ResponseWriter, r *http.Request) {
	payload := map[string]string{
		"status":  "ok",
		"env":     s.cfg.Env,
		"version": npadmin.Version,
		"commit":  npadmin.Commit,
	}

	s.JSON(w, r, http.StatusOK, envelope{"healthcheck": payload})
}
