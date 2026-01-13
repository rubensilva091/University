package pdf

import (
	"fmt"
	"time"

	"github.com/go-pdf/fpdf"
	"github.com/invisiblelab-dev/npadmin"
)

// GenerateTicketPDF cria um PDF para o bilhete e retorna o caminho do ficheiro gerado.
func GenerateTicketPDF(ticket *npadmin.PurchasedTicket, eventName string, purchaserName string) (string, error) {
	pdf := fpdf.New("P", "mm", "A4", "")
	pdf.AddPage()

	// Cabeçalho
	pdf.SetFont("Arial", "B", 24)
	pdf.Cell(40, 10, "Bilhete Eletronico")
	pdf.Ln(20)

	// Detalhes do Evento
	pdf.SetFont("Arial", "B", 16)
	pdf.Cell(40, 10, "Evento:")
	pdf.SetFont("Arial", "", 16)
	pdf.Cell(40, 10, eventName)
	pdf.Ln(12)

	// Detalhes do Bilhete
	pdf.SetFont("Arial", "B", 14)
	pdf.Cell(40, 10, "Bilhete ID:")
	pdf.SetFont("Arial", "", 14)
	pdf.Cell(40, 10, fmt.Sprintf("#%d", ticket.ID))
	pdf.Ln(10)

	pdf.SetFont("Arial", "B", 14)
	pdf.Cell(40, 10, "Comprador:")
	pdf.SetFont("Arial", "", 14)
	pdf.Cell(40, 10, purchaserName)
	pdf.Ln(10)

	pdf.SetFont("Arial", "B", 14)
	pdf.Cell(40, 10, "Data de Compra:")
	pdf.SetFont("Arial", "", 14)
	pdf.Cell(40, 10, ticket.UpdatedAt.Format("02/01/2006 15:04"))
	pdf.Ln(20)

	// Adicionar QR Code (Se tivermos o caminho do ficheiro gerado anteriormente)
	// Nota: Isto assume que o QR Code foi guardado em disco antes. 
	// Se não tivermos, podemos saltar esta parte ou gerar aqui.
	
	// Rodapé
	pdf.SetFont("Arial", "I", 10)
	pdf.Cell(0, 10, fmt.Sprintf("Gerado em %s", time.Now().Format(time.RFC1123)))

	// Definir caminho do ficheiro
	fileName := fmt.Sprintf("./.qrcodes/ticket_%d.pdf", ticket.ID)
	
	// Guardar PDF
	err := pdf.OutputFileAndClose(fileName)
	if err != nil {
		return "", fmt.Errorf("failed to save PDF: %w", err)
	}

	return fileName, nil
}