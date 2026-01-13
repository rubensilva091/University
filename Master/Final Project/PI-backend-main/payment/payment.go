package payment

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"time"
)

type Payment struct {
	MBKey           string
	MBWayKey        string
	MBURL           string
	MBWayURL        string
	Entidade        int
	SubEntidade     int
	AntiPhishingKey string
}

type PaymentParams struct {
	Category string `json:"category"`
	Period   int64  `json:"period"`
}

type MultibancoParams struct {
	PaymentParams
}

func (p *Payment) post(recipient string, payload []byte, headerValue string) ([]byte, error) {
	ctx := context.Background()
	//nolint:mnd // 3 second payment API timeout
	ctx, cancel := context.WithTimeout(ctx, 3*time.Second)
	defer cancel()

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, recipient, bytes.NewBuffer(payload))
	if err != nil {
		return nil, fmt.Errorf("failed to setup payment api request: %w", err)
	}

	req.Header.Set("Content-Type", headerValue)
	client := &http.Client{}

	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to post payment provider: %w", err)
	}

	defer func() {
		_ = resp.Body.Close()
	}()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Fatalln(err)
	}

	return body, nil
}
