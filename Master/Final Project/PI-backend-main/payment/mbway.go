// Package payment contains payment channel integrations.
package payment

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
)

// MBWayPayment is the MBWay payment response payload.
type MBWayPayment struct {
	RequestID      string `json:"IdPedido"`
	Amount         string `json:"Valor"`
	CurrencyCode   string `json:"CodigoMoeda"`
	Status         string `json:"Estado"`
	Date           string `json:"DataHora"`
	OrderID        string `json:"OrderId"`
	MsgDescription string `json:"MsgDescricao"`
}

// ReqMBWayStatus represents the MBWay status request.
type ReqMBWayStatus struct {
	XMLName  xml.Name
	XMLNS    string      `xml:"xmlns,attr"`
	MBWayKey string      `xml:"MbWayKey"`
	Channel  string      `xml:"canal"`
	IDs      []PaymentID `xml:"idspagamento"`
	MBWayURL string      `xml:"-"`
}

// MBWayStatus represents the MBWay status response payload.
type MBWayStatus struct {
	StatusRequests []RequestStatus `json:"EstadoPedidos"`
	StatusCode     string          `json:"Estado"`
	ReqTime        string          `json:"DataHora"`
	Description    string          `json:"MsgDescricao"`
}

// RequestStatus represents the status of an individual MBWay request.
type RequestStatus struct {
	RequestID   string `json:"IdPedido"`
	StatusCode  string `json:"Estado"`
	StartTime   string `json:"DataHoraPedidoRegistado"`
	UpdateTime  string `json:"DataHoraPedidoAtualizado"`
	Description string `json:"MsgDescricao"`
}

// PaymentID wraps an XML element with the request id.
type PaymentID struct {
	RequestID string `xml:"string"`
}

// ReqMBWayPayment represents the MBWay payment request body.
type ReqMBWayPayment struct {
	XMLName     xml.Name
	XMLNS       string `xml:"xmlns,attr"`
	MBWayKey    string `xml:"MbWayKey"`
	Channel     string `xml:"canal"`
	OrderID     string `xml:"referencia"`
	Amount      string `xml:"valor"`
	TLM         string `xml:"nrtlm"`
	Email       string `xml:"email"`
	Description string `xml:"descricao"`
	MBWayURL    string `xml:"-"`
}

type PaymentStatus int

const (
	Success PaymentStatus = iota
	UserCancelled
	MerchantCancelled
)

//nolint:gochecknoglobals // Payment status mapping is a constant lookup table
var paymentStatusMap = map[string]PaymentStatus{
	"000": Success,
	"020": UserCancelled,
	"048": MerchantCancelled,
}

func ParsePaymentStatus(str string) (PaymentStatus, bool) {
	c, ok := paymentStatusMap[str]

	return c, ok
}

func (p Payment) MBWayPayment(reqMBWayPaym ReqMBWayPayment) (MBWayPayment, error) {
	var res MBWayPayment
	reqMBWayPaym.XMLName = xml.Name{Local: "SetPedidoJSON"}
	reqMBWayPaym.XMLNS = "https://www.ifthenpay.com/"

	payloadBytes, err := xml.Marshal(reqMBWayPaym)
	if err != nil {
		return res, fmt.Errorf("failed to marshal mb way payment request payload: %w", err)
	}

	xmlPayload := fmt.Sprintf(`<?xml version="1.0" encoding="utf-8"?>
	<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
	    <soap:Body>
				%s
	    </soap:Body>
	</soap:Envelope>`, payloadBytes)

	payload := []byte(xmlPayload)
	var resp []byte
	if resp, err = p.post("https://mbway.ifthenpay.com/ifthenpaymbw.asmx", payload, "text/xml"); err != nil {
		return res, fmt.Errorf("failed to send mb way payment request: %w", err)
	}

	err = json.Unmarshal(resp, &res)
	if err != nil {
		return res, fmt.Errorf("failed to unmarshall mbway payment: %w", err)
	}
	return res, nil
}

func (p Payment) MBWayStatus(payment ReqMBWayStatus) (MBWayStatus, error) {
	var res MBWayStatus
	payment.XMLName = xml.Name{Local: "EstadoPedidosJSON"}
	payment.XMLNS = "https://www.ifthenpay.com/"

	payloadBytes, err := xml.Marshal(payment)
	if err != nil {
		return res, fmt.Errorf("failed to marshal mb way payment request payload: %w", err)
	}

	xmlPayload := fmt.Sprintf(`<?xml version="1.0" encoding="utf-8"?>
	<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
	    <soap:Body>
				%s
	    </soap:Body>
	</soap:Envelope>`, payloadBytes)

	payload := []byte(xmlPayload)
	var resp []byte
	if resp, err = p.post("https://mbway.ifthenpay.com/ifthenpaymbw.asmx", payload, "text/xml"); err != nil {
		return res, fmt.Errorf("failed to get mbway status request: %w", err)
	}

	err = json.Unmarshal(resp, &res)
	if err != nil {
		return res, fmt.Errorf("failed to unmarshall mbway payment status: %w", err)
	}
	return res, nil
}
