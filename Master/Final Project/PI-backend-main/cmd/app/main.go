package main

import (
	"context"
	"errors"
	"flag"
	"log"
	"os"
	"os/signal"
	"path"
	"path/filepath"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/http"
	"github.com/invisiblelab-dev/npadmin/mailer"
	"github.com/invisiblelab-dev/npadmin/payment"
	"github.com/invisiblelab-dev/npadmin/postgres"
	"github.com/invisiblelab-dev/npadmin/validation"
	"github.com/pusher/pusher-http-go/v5"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

// nolint
var (
	version string
	commit  string
)

func main() {
	npadmin.Version = version
	npadmin.Commit = commit

	var cfgFilePath string
	flag.StringVar(&cfgFilePath, "config", "etc/npadmin.config", "path to configuration file")
	flag.Parse()

	cfg := npadmin.LoadConfiguration(cfgFilePath)

	ctx, cancel := context.WithCancel(context.Background())
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	go func() { <-c; cancel() }()

	zapcfg := zap.NewProductionConfig()
	zapcfg.OutputPaths = []string{cfg.Logger.Path, "stderr"}
	zapcfg.Level = zap.NewAtomicLevelAt(zapcore.DebugLevel)
	zapcfg.EncoderConfig.EncodeTime = zapcore.TimeEncoder(func(t time.Time, enc zapcore.PrimitiveArrayEncoder) {
		enc.AppendString(t.UTC().Format(time.RFC3339))
	})

	if _, err := os.Stat(".qrcodes"); errors.Is(err, os.ErrNotExist) {
		//nolint:mnd // 0750 is standard directory permission
		err := os.Mkdir(".qrcodes", 0750)
		if err != nil {
			log.Fatal(err)
		}
	}

	abs, _ := filepath.Abs(cfg.Logger.Path)
	//nolint:mnd // 0750 is standard directory permission
	err := os.MkdirAll(path.Dir(abs), 0750)
	if err != nil && !os.IsExist(err) {
		log.Fatal(err)
	}

	logger, err := zapcfg.Build()
	if err != nil {
		log.Fatalf("Failed to build zap logger: %v", err)
	}

	zap.ReplaceGlobals(logger)

	// --- Database Connection ---
	db := postgres.Connect(postgres.Config{
		Dsn:          cfg.Db.Dsn,
		MaxOpenConns: cfg.Db.MaxOpenConns,
		MaxIdleConns: cfg.Db.MaxIdleConns,
		MaxIdleTime:  cfg.Db.MaxIdleTime,
	})
	if db == nil || db.DB == nil || db.DB.DB == nil {
		logger.Fatal("failed to get underlying *sql.DB from postgres connection")
	}

	// --- Mailer Setup ---
	mailClient := mailer.New(&mailer.Config{
		Host:     cfg.Mailer.Host,
		Port:     cfg.Mailer.Port,
		Username: cfg.Mailer.Username,
		Password: cfg.Mailer.Password,
		Sender:   cfg.Mailer.Sender,
	})

	// --- HTTP Server Setup ---
	httpServer := http.New(&http.Config{
		Addr:            cfg.HTTP.Addr,
		Fqdn:            cfg.HTTP.Fqdn,
		Port:            cfg.HTTP.Port,
		ListenPort:      cfg.HTTP.ListenPort,
		ReadTimeout:     cfg.HTTP.ReadTimeout,
		WriteTimeout:    cfg.HTTP.WriteTimeout,
		ShutdownTimeout: cfg.HTTP.ShutdownTimeout,

		TLS:                  cfg.HTTP.TLS,
		Env:                  cfg.Env,
		JwtPrivateKey:        cfg.HTTP.JwtPrivateKey,
		JwtPublicKey:         cfg.HTTP.JwtPublicKey,
		JwtRefreshPrivateKey: cfg.HTTP.JwtRefreshPrivateKey,
		JwtRefreshPublicKey:  cfg.HTTP.JwtRefreshPublicKey,
		Storage:              cfg.Storage,
		WebApp:               cfg.WebApp,
	})

	// --- Pass DB to HTTP Server ---
	httpServer.SetDB(db.DB.DB)

	// --- Payment Setup ---
	paymentCfg := payment.Payment{
		MBKey:           cfg.Payment.MBKey,
		MBWayKey:        cfg.Payment.MBWayKey,
		MBURL:           cfg.Payment.MBURL,
		MBWayURL:        cfg.Payment.MBWayURL,
		Entidade:        cfg.Payment.Entidade,
		SubEntidade:     cfg.Payment.SubEntidade,
		AntiPhishingKey: cfg.Payment.AntiPhishingKey,
	}

	// --- Create Admin ---
	// TODO: Esta função precisa ser atualizada para lidar com tenant_id
	err = postgres.CreateAdmin(cfg.Admin.Email, cfg.Admin.NIF, db)
	if err != nil {
		logger.Warn("failed to create default admin account", zap.Error(err))
	}

	// --- Service Instantiation ---
	accountConfirmationService := postgres.NewAccountConfirmationService(db)
	resetPasswordService := postgres.NewResetPasswordService(db)
	updateEmailService := postgres.NewUpdateEmailService(db)

	accountService := postgres.NewAccountService(db)
	accountService.AccountConfirmationService = accountConfirmationService
	accountService.ResetPasswordService = resetPasswordService
	accountService.UpdateEmailService = updateEmailService

	paymentService := postgres.NewPaymentService(db)

	authenticationConfirmationService := postgres.NewAuthenticationConfirmationService(db)
	authenticationService := postgres.NewAuthenticationService(db)
	authenticationService.AuthenticationConfirmationService = authenticationConfirmationService

	authorizationService := postgres.NewAuthorizationService(db)
	profileService := postgres.NewProfileService(db)
	notifyService := postgres.NewNotifyService(db)
	dataService := postgres.NewDataService(db)
	tenantService := postgres.NewTenantService(db)

	eventService := postgres.NewEventService(db, paymentService, paymentCfg)

	// --- JOB: Verificação de Validade da Quota ---
	go func() {
		// Aguardar alguns segundos para garantir que o sistema inicializou
		time.Sleep(10 * time.Second)

		logger.Info("Starting subscription expiration checker job")

		check := func() {
			// DEFINIÇÃO: Avisar se expirar nos próximos 15 dias
			daysBeforeExpiration := 15

			subs, err := paymentService.GetExpiringSubscriptions(db, daysBeforeExpiration)
			if err != nil {
				logger.Error("Job: Failed to get expiring subscriptions", zap.Error(err))
				return
			}

			if len(subs) > 0 {
				logger.Info("Job: Found expiring subscriptions", zap.Int("count", len(subs)))
			}

			for _, sub := range subs {
				// Estrutura de dados para o template
				data := struct {
					EndDate      string
					DashboardURL string
				}{
					EndDate:      sub.EndDate.Format("02-01-2006"),
					DashboardURL: cfg.WebApp.AssociateDashboard,
				}

				// Enviar Email
				mailClient.SendExpirationWarning(sub.Email, data)

				// Marcar como enviado para não repetir
				if err := paymentService.MarkWarningSent(db, sub.ID); err != nil {
					logger.Error("Job: Failed to mark warning sent", zap.Int64("sub_id", sub.ID), zap.Error(err))
				} else {
					logger.Info("Job: Expiration warning sent", zap.String("email", sub.Email))
				}
			}
		}

		// Executar imediatamente ao arrancar
		check()

		// Executar periodicamente (a cada 24 horas)
		ticker := time.NewTicker(24 * time.Hour)
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				logger.Info("Stopping subscription expiration checker job")
				return
			case <-ticker.C:
				check()
			}
		}
	}()

	// --- Assign Services to HTTP Server ---
	httpServer.AccountService = accountService
	httpServer.AccountConfirmationService = accountConfirmationService
	httpServer.PaymentService = paymentService
	httpServer.AuthenticationService = authenticationService
	httpServer.AuthorizationService = authorizationService
	httpServer.ProfileService = profileService
	httpServer.NotifyService = notifyService
	httpServer.DataService = dataService
	httpServer.TenantService = tenantService
	httpServer.EventService = eventService
	httpServer.Mailer = mailClient
	httpServer.Log = logger
	httpServer.Payment = paymentCfg

	httpServer.PusherClient = pusher.Client{
		AppID:   cfg.Pusher.AppID,
		Key:     cfg.Pusher.Key,
		Secret:  cfg.Pusher.Secret,
		Cluster: cfg.Pusher.Cluster,
		Secure:  true,
	}

	// --- Validator Setup ---
	httpServer.StoreValidator = validation.New(db, logger)
	httpServer.RegisterValidator()

	// --- Run Server ---
	if err := httpServer.Run(); err != nil {
		logger.Fatal("failed to start http server", zap.Error(err))
	}

	// nolint
	defer logger.Sync()

	logger.Info("Server started", zap.String("addr", cfg.HTTP.Addr), zap.Int("port", cfg.HTTP.ListenPort))

	// --- Wait for shutdown ---
	<-ctx.Done()

	logger.Info("Terminating...")

	// --- Cleanup ---
	if err := db.Close(); err != nil {
		logger.Error("Error closing database connection", zap.Error(err))
	}

	if err := httpServer.Close(); err != nil {
		logger.Error("Error closing server connections", zap.Error(err))
	}
}