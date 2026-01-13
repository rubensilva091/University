package mailer

import (
	"bytes"
	"embed"
	"fmt"
	"path"
	"text/template"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"go.uber.org/zap"
	"gopkg.in/mail.v2"
)

//go:embed "templates"
var templateFS embed.FS

type Mailer struct {
	dialer *mail.Dialer
	sender string
	logger *zap.Logger
}

type Config struct {
	Host     string
	Port     int
	Username string
	Password string
	Sender   string
}

type Attachment struct {
	Data string
}

func New(cfg *Config) Mailer {
	dialer := mail.NewDialer(cfg.Host, cfg.Port, cfg.Username, cfg.Password)
	// AUMENTAR TIMEOUT: 15 segundos para dar tempo de enviar anexos
	dialer.Timeout = 15 * time.Second

	return Mailer{
		dialer: dialer,
		sender: cfg.Sender,
		logger: zap.L(),
	}
}

func (m Mailer) Send(recipient, templateFile string, data any, attachment Attachment) error {
	tmpl, err := template.New("email").ParseFS(templateFS, path.Join("templates", templateFile))
	if err != nil {
		return fmt.Errorf("failed to parse email template: %w", err)
	}

	subject := new(bytes.Buffer)
	if err = tmpl.ExecuteTemplate(subject, "subject", data); err != nil {
		return fmt.Errorf("failed to hydrate subject email template: %w", err)
	}

	plaintext := new(bytes.Buffer)
	if err = tmpl.ExecuteTemplate(plaintext, "plain", data); err != nil {
		return fmt.Errorf("failed to hydrate plaintext email template: %w", err)
	}

	html := new(bytes.Buffer)
	if err = tmpl.ExecuteTemplate(html, "html", data); err != nil {
		return fmt.Errorf("failed to hydrate html email template: %w", err)
	}

	msg := mail.NewMessage()
	msg.SetHeader("To", recipient)
	msg.SetHeader("From", m.sender)
	msg.SetHeader("Subject", subject.String())
	msg.SetBody("text/plain", plaintext.String())
	msg.AddAlternative("text/html", html.String())

	// CORREÇÃO: Usar Embed para o logótipo (inline) com o Content-ID correto
	// Isto evita partir a estrutura dos anexos seguintes.
	msg.Embed("./mailer/header/logo.png", mail.SetHeader(map[string][]string{"Content-ID": {"<logo.png>"}}))

	// Anexar o PDF se existir
	if attachment.Data != "" {
		msg.Attach(attachment.Data)
	}

	if err = m.dialer.DialAndSend(msg); err != nil {
		return fmt.Errorf("failed to send email: %w", err)
	}
	return nil
}

// SendEventTicketEmail envia o email com o bilhete em anexo
func (m Mailer) SendEventTicketEmail(destination string, data any, pdfPath string) {
	var atch Attachment
	atch.Data = pdfPath // Caminho do PDF
	
	go func() {
		if err := m.Send(destination, "ticket.tmpl", data, atch); err != nil {
			m.logger.Info("Failed to send ticket email", zap.String("recipient", destination), zap.Error(err))
		}
	}()
}

func (m Mailer) SendWelcomeEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "welcome.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send welcome email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) ResendWelcomeEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "welcome.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to resend welcome email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) SendPasswordResetEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "recover.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send recover email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) SendEmailUpdateEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "recover_email.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send update email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) SendNotificationEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "change-email.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send update email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) SendLoginEmail(destination string, view npadmin.AccountView) {
	var atch Attachment
	view.Date = time.Now().Format("2006-01-02 15:04:05")
	go func() {
		if err := m.Send(destination, "login-email.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send update email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

func (m Mailer) SendAssociateCard(destination string, card npadmin.AccountCard) {
	var atch Attachment
	atch.Data = card.QRCode
	go func() {
		if err := m.Send(destination, "card.tmpl", &card, atch); err != nil {
			m.logger.Info("Failed to send update email", zap.Int64("account", card.AssociateNumber), zap.Error(err))
		}
	}()
}

func (m Mailer) SendPaymentSuccessful(destination string, view npadmin.AccountView) {
	var atch Attachment
	go func() {
		if err := m.Send(destination, "payment-success.tmpl", &view, atch); err != nil {
			m.logger.Info("Failed to send success payment email", zap.Int64("account", view.Account.ID), zap.Error(err))
		}
	}()
}

// SendExpirationWarning envia o email de aviso de expiração de quota
func (m Mailer) SendExpirationWarning(destination string, data any) {
	var atch Attachment
	go func() {
		// Usa o template expiration_warning.tmpl criado anteriormente
		if err := m.Send(destination, "expiration_warning.tmpl", data, atch); err != nil {
			m.logger.Info("Failed to send expiration warning email", zap.String("recipient", destination), zap.Error(err))
		}
	}()
}