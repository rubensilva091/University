package http

import (
	"fmt"
	"net/http"
	"strconv"

)

// setHeaders remains the same
func setHeaders(w http.ResponseWriter, name string, length int) {
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, name))
	w.Header().Set("Content-Length", strconv.Itoa(length))
}
