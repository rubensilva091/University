package http

import (
	"context"
	"database/sql" // Import standard SQL package
	"errors"       // Import errors
	"fmt"
	"net"
	"net/http"
	"strings"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/go-chi/cors"
	"github.com/go-playground/validator/v10"
	"github.com/gorilla/schema"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	mw "github.com/invisiblelab-dev/npadmin/http/middleware" // Use mw alias
	"github.com/invisiblelab-dev/npadmin/http/resource"
	"github.com/invisiblelab-dev/npadmin/mailer"
	"github.com/invisiblelab-dev/npadmin/payment"
	"github.com/invisiblelab-dev/npadmin/validation"
	"github.com/pusher/pusher-http-go/v5"
	"go.uber.org/zap"
)


type Config struct {
	Addr            string
	Fqdn            string
	Port            int
	ListenPort      int
	ReadTimeout     int
	WriteTimeout    int
	ShutdownTimeout int

	Env                  string
	TLS                  bool
	JwtPrivateKey        string
	JwtPublicKey         string
	JwtRefreshPrivateKey string
	JwtRefreshPublicKey  string
	Storage              npadmin.Storage
	WebApp               npadmin.WebApp
}

// Server struct now includes TenantService
type Server struct {
	listener net.Listener
	server   *http.Server
	router   *chi.Mux
	cfg      *Config
	decoder  *schema.Decoder
	validate *validator.Validate
	db       *sql.DB // Database connection pool

	Log                               *zap.Logger
	AccountService                    npadmin.AccountService
	AccountConfirmationService        npadmin.AccountConfirmationService
	AuthenticationService             npadmin.AuthenticationService
	AuthenticationConfirmationService npadmin.AuthenticationConfirmationService
	AuthorizationService              npadmin.AuthorizationService
	AdminService                      npadmin.AdminService // Keep if used elsewhere
	ProfileService                    npadmin.ProfileService
	NotifyService                     npadmin.NotifyService // Keep if used elsewhere
	DataService                       npadmin.DataService
	PaymentService                    npadmin.PaymentService
	TenantService                     npadmin.TenantService // <-- ADICIONADO
	EventService                      npadmin.EventService // <-- ADICIONAR ESTA LINHA
	StoreValidator                    *validation.StoreValidator
	Mailer                            mailer.Mailer
	ResourceServices                  resource.Services // Keep if used elsewhere
	Payment                           payment.Payment
	PusherClient                      pusher.Client
}

// New creates a new HTTP server instance.
func New(cfg *Config) *Server {
	srv := &Server{
		server: &http.Server{
			ReadTimeout:       time.Duration(cfg.ReadTimeout) * time.Second,
			WriteTimeout:      time.Duration(cfg.WriteTimeout) * time.Second,
			ReadHeaderTimeout: time.Duration(cfg.ReadTimeout) * time.Second,
		},
		router:   chi.NewRouter(),
		cfg:      cfg,
		decoder:  schema.NewDecoder(),
		validate: validator.New(),
		// db field will be set later by SetDB
	}
	srv.server.Handler = srv.router
	return srv
}

// SetDB sets the database connection for the server.
func (s *Server) SetDB(db *sql.DB) {
	s.db = db
}

// RegisterValidator registers custom validation functions.
func (s *Server) RegisterValidator() {
	// Register type handlers
	s.validate.RegisterCustomTypeFunc(validation.Password, crypto.Password{})     
	s.validate.RegisterCustomTypeFunc(validation.NullString, npadmin.NullString{})

	// Register validation tags
	if err := s.validate.RegisterValidation("acceptance", validation.Acceptance); err != nil {
		s.Log.Fatal("Unable to register validator", zap.String("name", "acceptance"))
	}
	if err := s.validate.RegisterValidation("has-symbols", validation.HasSymbols); err != nil {
		s.Log.Fatal("Unable to register has-symbols validator", zap.String("name", "has-symbols"))
	}
	if err := s.validate.RegisterValidation("has-numbers", validation.HasNumbers); err != nil {
		s.Log.Fatal("Unable to register has-numbers validator", zap.String("name", "has-numbers"))
	}
	// Register GLOBAL email uniqueness check
	if err := s.validate.RegisterValidation("email-uniqueness", s.StoreValidator.EmailUnique); err != nil {
		s.Log.Fatal("Unable to register email-uniqueness validator", zap.String("name", "email-uniqueness"))
	}
	// NIF uniqueness is now checked manually in the handler
}

// Host returns the server's host string, omitting standard ports.
func (s *Server) Host() string {
	if s.cfg.Port == 80 || s.cfg.Port == 443 {
		return s.cfg.Fqdn
	}
	return fmt.Sprintf("%s:%d", s.cfg.Fqdn, s.cfg.Port)
}

// Schema returns the URL schema (http or https) based on TLS config.
func (s *Server) Schema() string {
	if s.cfg.TLS { return "https" }
	return "http"
}

// URI returns the full base URI for the server.
func (s *Server) URI() string {
	return fmt.Sprintf("%s://%s", s.Schema(), s.Host())
}

// Run sets up middleware, routes, and starts the HTTP server.
func (s *Server) Run() error {
	var err error

	// Assign resource services if needed
	s.ResourceServices.AccountService = s.AccountService
	s.ResourceServices.ProfileService = s.ProfileService

	// Register middleware
	s.router.Use(middleware.RequestID)
	s.router.Use(middleware.RealIP)
	s.router.Use(cors.Handler(cors.Options{
		AllowedOrigins:   []string{"https://*", "http://*"},
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{"Accept", "Authorization", "Content-Type", "X-CSRF-Token", "X-Tenant-ID"}, // Ensure X-Tenant-ID is allowed
		ExposedHeaders:   []string{"Link"},
		AllowCredentials: false,
		MaxAge:           300,
	}))
	s.router.Use(mw.NewZap(s.Log))
	s.router.Use(middleware.Recoverer)
	s.router.Use(middleware.Timeout(60 * time.Second))

	// Register routes under /api/v1
	s.router.Route("/api/v1", func(r chi.Router) {
		s.registerHealthCheckRoutes(r)   
		s.registerAccountRoutes(r)       
		s.registerAuthenticationRoutes(r)
		s.registerAuthorizationRoutes(r) 
		s.registerDataAdminRoutes(r)     
		s.registerPaymentRoutes(r)       
		s.registerAssociateRoutes(r)     
		s.registerTenantRoutes(r)         // <-- ADICIONADO
		s.registerEventRoutes(r)
		        
	})

	// Register callback routes at the root
	s.router.Route("/", func(r chi.Router) {
		s.registerCallbackRoutes(r)
	})

	// Log registered routes
	walkFunc := func(
		method string,
		route string,
		handler http.Handler,
		middlewares ...func(http.Handler) http.Handler) error {
		route = strings.ReplaceAll(route, "/*/", "/") // Clean up path slashes
		s.Log.Info("route", zap.String("method", method), zap.String("path", route))
		return nil
	}
	if err := chi.Walk(s.router, walkFunc); err != nil {
		s.Log.Error("failed to walk routes", zap.Error(err))
	}

	// Start listening
	address := fmt.Sprintf("%s:%d", s.cfg.Addr, s.cfg.ListenPort)
	s.listener, err = net.Listen("tcp", address)
	if err != nil {
		return fmt.Errorf("failed to listen on address: '%s': %w", address, err)
	}

	// Run server in goroutine
	go func() {
		err = s.server.Serve(s.listener)
		// Report errors unless it's ErrServerClosed (graceful shutdown)
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			s.Log.Error("HTTP server error", zap.Error(err))
		}
	}()

	return nil // Return nil on successful start
}

// Close gracefully shuts down the HTTP server.
func (s *Server) Close() error {
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(s.cfg.ShutdownTimeout)*time.Second)
	defer cancel()
	return s.server.Shutdown(ctx)
}