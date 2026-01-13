package payment

import (
	"fmt"
	"strconv"
	"strings"
)

type MBReference struct {
	Amount    string `json:"Amount"`
	Entity    string `json:"Entity"`
	Reference string `json:"Reference"`
	OrderID   string `json:"OrderId"`
	Status    string `json:"Status"`
}

type ReqMBReference struct {
	Entidade    int     `json:"entidade"`
	SubEntidade int     `json:"subentidade"`
	AccountID   int64   `json:accountId"`
	MBKey       string  `json:"mbKey"`
	OrderID     int64   `json:"orderId"`
	Amount      float64 `json:"amount"`
	MBURL       string  `json:"-"`
}

func (p Payment) MBReference(reqMBRef ReqMBReference) (MBReference, error) {
	var res MBReference
	result, err := generate(reqMBRef.Entidade, reqMBRef.SubEntidade, int(reqMBRef.OrderID), reqMBRef.Amount)
	if err != nil {
		return res, err
	}

	amount := fmt.Sprintf("%f", reqMBRef.Amount)
	mbReference := MBReference{
		Entity:    fmt.Sprintf("%d", reqMBRef.Entidade),
		Reference: fmt.Sprintf("%d%s%s", reqMBRef.SubEntidade, fmt.Sprintf("%04d", int(reqMBRef.OrderID)), result),
		Amount:    amount,
	}
	return mbReference, nil
}

func generate(entity int, subentity int, orderID int, value float64) (string, error) {
	result := 0
	ops := []int{51, 73, 17, 89, 38, 62, 45, 53, 15, 50, 5, 49, 34, 81, 76, 27, 90, 9, 30, 3}

	var digits strings.Builder
	digits.WriteString(strconv.Itoa(entity))
	digits.WriteString(strconv.Itoa(subentity))
	digits.WriteString(fmt.Sprintf("%04d", orderID))
	//nolint:mnd // Convert euros to cents (multiply by 100)
	digits.WriteString(fmt.Sprintf("%08d", int(value*100)))

	for idx, chr := range digits.String() {
		digit, err := strconv.Atoi(string(chr))
		if err != nil {
			return "", fmt.Errorf("failed convert to digit: %w", err)
		}
		result += ops[idx] * digit
	}

	//nolint:mnd // Multibanco checksum algorithm constants (98 and 97)
	final := fmt.Sprintf("%02d", 98-(result%97))

	return final, nil
}
