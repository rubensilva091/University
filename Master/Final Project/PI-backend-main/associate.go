package npadmin

type AssociateService interface {
	UpdateConfig(*ConfigParams) (*AdminConfig, error)

	Get() (*AdminConfig, error)
}

type ScanAccount struct {
	ID int64
}
