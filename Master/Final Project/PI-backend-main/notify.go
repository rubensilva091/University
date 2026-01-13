package npadmin

type NotifyPayload struct {
	Payload string
}

type NotifyService interface {
	// CHANGED: Added Queryable
	Create(Queryable, *NotifyPayload) error
}