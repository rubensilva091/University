package npadmin

import (
	"log"
	"os"

	"github.com/BurntSushi/toml"
)

type Logger struct {
	Level string `toml:"level"`
	Path  string `toml:"path"`
}

type Db struct {
	Dsn          string `toml:"dsn"`
	MaxOpenConns int    `toml:"max-open-connections"`
	MaxIdleConns int    `toml:"max-idle-connections"`
	MaxIdleTime  string `toml:"max-idle-time"`
}

type HTTP struct {
	Addr                 string `toml:"address"`
	Fqdn                 string `toml:"fqdn"`
	Port                 int    `toml:"port"`
	ListenPort           int    `toml:"listen-port"`
	ReadTimeout          int    `toml:"read-timeout"`
	WriteTimeout         int    `toml:"write-timeout"`
	ShutdownTimeout      int    `toml:"shutdown-timeout"`
	TLS                  bool   `toml:"tls"`
	JwtPrivateKey        string `toml:"jwt-private"`
	JwtPublicKey         string `toml:"jwt-public"`
	JwtRefreshPrivateKey string `toml:"jwt-refresh-private"`
	JwtRefreshPublicKey  string `toml:"jwt-refresh-public"`
}

type Mailer struct {
	Host     string `toml:"host"`
	Port     int    `toml:"port"`
	Username string `toml:"username"`
	Password string `toml:"password"`
	Sender   string `toml:"sender"`
}

type Storage struct {
	Path string `toml:"path"`
}

type WebApp struct {
	AdminDashboard     string `toml:"admin-dashboard-url"`
	AssociateDashboard string `toml:"associate-dashboard-url"`
	AccountSuccessPage string `toml:"account-success-page-url"`
	AccountErrorPage   string `toml:"account-error-page-url"`
	ScanAssociatePage  string `toml:"scan-associate-url"`
	LoginErrorPage     string `toml:"login-error-page-url"`
}

type Payment struct {
	MBKey           string `toml:"multibanco-key"`
	MBWayKey        string `toml:"mbway-key"`
	MBURL           string `toml:"multibanco-url"`
	MBWayURL        string `toml:"mbway-url"`
	Entidade        int    `toml:"entidade"`
	SubEntidade     int    `toml:"subentidade"`
	AntiPhishingKey string `toml:"anti-phishing-key"`
}

type Pusher struct {
	AppID   string `toml:"app-id"`
	Key     string `toml:"key"`
	Secret  string `toml:"secret"`
	Cluster string `toml:"cluster"`
}

type Config struct {
	Env     string  `toml:"env"`
	HTTP    HTTP    `toml:"http"`
	Db      Db      `toml:"database"`
	Mailer  Mailer  `toml:"mailer"`
	Storage Storage `toml:"storage"`
	WebApp  WebApp  `toml:"webapp"`
	Logger  Logger  `toml:"logger"`
	Payment Payment `toml:"payment"`
	Pusher  Pusher  `toml:"pusher"`
	Admin   Admin   `toml:"admin"`
}

type Admin struct {
	Email string `toml:"email"`
	NIF   string `toml:"nif"`
}

func LoadConfiguration(cfgFilePath string) Config {
	var config Config
	if buf, err := os.ReadFile(cfgFilePath); err != nil {
		log.Fatalf("Config file not found: %s", cfgFilePath)
	} else if err := toml.Unmarshal(buf, &config); err != nil {
		log.Fatalf("Unable to parse configuration file: %v", err)
	}

	return config
}
