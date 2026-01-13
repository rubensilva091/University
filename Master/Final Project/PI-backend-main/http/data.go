package http

import (
	"bytes"        // Import bytes
	"database/sql"
	"encoding/csv" // Import encoding/csv
	"encoding/json" // Import encoding/json
	"errors"
	"fmt" // Import fmt
	"io"  // Import io
	"net/http"
	"strconv" // Importar strconv
	"strings" // Import strings
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-playground/validator/v10" // Importar validator
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware" // Usar alias
)

func (s *Server) registerDataAdminRoutes(r chi.Router) {
	r.Route("/admin/data", func(r chi.Router) {
		r.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
		r.Use(middleware.JwtAuthorize(s.cfg.JwtPublicKey))
		r.Use(middleware.TenantTxMiddleware(s.db))
		r.Use(middleware.AccountCtx(s.AccountService))
		r.Use(middleware.AdminCtx(s.AccountService, s.ProfileService))

		r.Get("/associates/summary", s.handleAssociatesDataSummary)
		r.Get("/associates", s.handleAssociatesDataFilter)
		r.Get("/associates/extract", s.handleAssociatesExtract)
		r.Get("/associates/account", s.handleAssociateData)
		r.Get("/associates/account/extract", s.handleAssociateDataExtract)
		r.Put("/associates/account/disable", s.handleDisableAccount)
		r.Put("/associates/account/verify", s.handleVerifyAccount)
		r.Post("/associates/account/payment", s.handleInsertPayment)
		r.Put("/associates/account/invoice", s.handleUploadInvoice)
		r.Put("/associates/account/update/role", s.handleRoleChange)
		r.Put("/associates/account/update/profile", s.handleAssociateProfileUpdate)
		r.Get("/prices", s.handleGetPrices)
		r.Post("/prices", s.handleInsertPrice)
		r.Delete("/prices", s.handleDeletePrice)
	})
}

// handleAssociatesDataSummary busca resumos do tenant atual (via RLS em tx).
func (s *Server) handleAssociatesDataSummary(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	history, err := s.DataService.AssociatesHistory(tx)
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }
	active, inactive, err := s.DataService.SubscriptionsSituation(tx)
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }
	monthlyIncome, err := s.DataService.MonthlyIncome(tx)
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }
	havePaid, neverPaid, err := s.DataService.HistoricSubs(tx)
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }
	totalAssociates, err := s.DataService.TotalAssociates(tx)
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }

	s.JSON(w, r, http.StatusOK, envelope{
		"associatesHistory":      history,
		"subscriptionsSituation": envelope{"activeAssociates": active, "inactiveAssociates": inactive},
		"monthlyIncome":          monthlyIncome,
		"historicPayers":         envelope{"havePaid": havePaid, "neverPaid": neverPaid},
		"totalAssociates":        totalAssociates,
	})
}

// handleAssociatesDataFilter busca associados filtrados do tenant atual (via RLS em tx).
func (s *Server) handleAssociatesDataFilter(w http.ResponseWriter, r *http.Request) {
	var params npadmin.FilterData
	qs := r.URL.Query()
	if err := s.decoder.Decode(&params, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}
	if params.PageSize == 0 { params.PageSize = 10; params.Page = 1 }

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	// Chama a função que retorna lista e total
	filteredAssociates, total, err := s.DataService.FilteredAssociates(tx, &params)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{
		"filtered-query": filteredAssociates, 
		"total": total,
	})
}

// handleAssociateData busca dados completos de um associado no tenant atual (via RLS em tx).
func (s *Server) handleAssociateData(w http.ResponseWriter, r *http.Request) {
	var associateFilter npadmin.AssociateFilter
	qs := r.URL.Query()
	if err := s.decoder.Decode(&associateFilter, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}
	if err := s.validate.Struct(associateFilter); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	
	// A função AssociateCompleteData usa GetProfileByEmail que tem RLS na tabela 'profiles'.
	// Se o user não pertencer ao tenant, dá erro sql.ErrNoRows.
	associateData, err := s.DataService.AssociateCompleteData(tx, associateFilter)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "associate not found in this tenant"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"associateData": associateData})
}

// handleDisableAccount atualiza o status da conta GLOBAL. Requer admin do tenant atual.
func (s *Server) handleDisableAccount(w http.ResponseWriter, r *http.Request) {
	var associateID npadmin.AssociateID
	if err := DecodeJSON(w, r, &associateID); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(associateID); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	// 1. Obter Conta Global pelo Email
	account, err := s.AccountService.GetByEmail(tx, associateID.Email)
	if err != nil {
		s.JSON(w, r, http.StatusNotFound, envelope{"account": "account not found"})
		return
	}

	// 2. VERIFICAÇÃO DE SEGURANÇA: O utilizador pertence a este tenant?
	// Tentamos obter o perfil DENTRO do contexto RLS deste tenant.
	_, errProfile := s.ProfileService.Get(tx, account.ID)
	if errProfile != nil {
		if errors.Is(errProfile, sql.ErrNoRows) {
			// Tentativa de desativar alguém que não é deste tenant!
			s.LogWarn(r, fmt.Errorf("security alert: admin tried to disable user %s not in tenant", associateID.Email))
			s.JSON(w, r, http.StatusForbidden, envelope{"message": "user does not belong to this organization"})
		} else {
			s.LogError(r, errProfile)
			s.JSON(w, r, http.StatusInternalServerError, errProfile)
		}
		return
	}

	// 3. Executar Desativação (Global)
	err = s.AccountService.ExecuteAccountDisable(tx, associateID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"account": "disabled"})
}

// handleVerifyAccount atualiza o status da conta GLOBAL. Requer admin do tenant atual.
func (s *Server) handleVerifyAccount(w http.ResponseWriter, r *http.Request) {
	var associateID npadmin.AssociateID
	if err := DecodeJSON(w, r, &associateID); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(associateID); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	// 1. Obter Conta Global
	account, err := s.AccountService.GetByEmail(tx, associateID.Email)
	if err != nil {
		s.JSON(w, r, http.StatusNotFound, envelope{"account": "account not found"})
		return
	}

	// 2. VERIFICAÇÃO DE SEGURANÇA (Igual ao Disable)
	_, errProfile := s.ProfileService.Get(tx, account.ID)
	if errProfile != nil {
		if errors.Is(errProfile, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusForbidden, envelope{"message": "user does not belong to this organization"})
		} else {
			s.LogError(r, errProfile)
			s.JSON(w, r, http.StatusInternalServerError, errProfile)
		}
		return
	}

	// 3. Executar Verificação
	err = s.AccountService.ExecuteAccountVerify(tx, associateID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"account": "verified"})
}

// handleUploadInvoice opera nos dados do tenant atual (via RLS em tx).
func (s *Server) handleUploadInvoice(w http.ResponseWriter, r *http.Request) {
	var params npadmin.AddInvoice
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleUploadInvoice"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	err := s.DataService.UploadInvoice(tx, params.InvoiceURL, params.ID, params.SubscriptionID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"invoice": "uploaded"})
}

// handleInsertPrice opera nos dados do tenant atual (via RLS em tx).
func (s *Server) handleInsertPrice(w http.ResponseWriter, r *http.Request) {
	var params npadmin.AddPrice
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleInsertPrice"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	categoryID, err := s.AccountService.GetCategory(tx, params.Category)
	if err != nil {
		s.LogError(r, err)
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"category": "category not found in this tenant"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}
	err = s.DataService.InsertPrice(tx, categoryID, params.Period, params.Price)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"price": params})
}

// handleDeletePrice opera nos dados do tenant atual (via RLS em tx).
func (s *Server) handleDeletePrice(w http.ResponseWriter, r *http.Request) {
	var params npadmin.DeletePrice
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleDeletePrice"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	categoryID, err := s.AccountService.GetCategory(tx, params.Category)
	if err != nil {
		s.LogError(r, err)
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"category": "category not found in this tenant"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}
	deleted, err := s.DataService.DeletePrice(tx, categoryID, params.Period)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	if !deleted {
		s.JSON(w, r, http.StatusNotFound, envelope{"deleted": false, "message": "price definition not found for this category/period in this tenant"})
	} else {
		s.JSON(w, r, http.StatusOK, envelope{"deleted": deleted})
	}
}

// handleGetPrices opera nos dados do tenant atual (via RLS em tx).
func (s *Server) handleGetPrices(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleGetPrices"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	prices, err := s.DataService.AllPrices(tx)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"prices": prices})
}

// handleRoleChange atualiza o role na tabela 'account_tenant_memberships'.
func (s *Server) handleRoleChange(w http.ResponseWriter, r *http.Request) {
	var changeInfo struct {
		Email string `json:"email" validate:"required,email"`
		Role  string `json:"role" validate:"required,eq=admin|eq=associate"`
	}
	if err := DecodeJSON(w, r, &changeInfo); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(changeInfo); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleRoleChange"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	tenantIDStr, _ := r.Context().Value(middleware.CtxKeyTenantID).(string)
	tenantID, _ := strconv.ParseInt(tenantIDStr, 10, 64)
	accountToChange, err := s.AccountService.GetByEmail(tx, changeInfo.Email)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "user with the specified email not found"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "error finding user by email"})
		}
		return
	}
	err = s.ProfileService.UpdateMembershipRole(tx, accountToChange.ID, tenantID, changeInfo.Role)
	if err != nil {
		s.LogError(r, err)
		if strings.Contains(err.Error(), "membership not found") || strings.Contains(err.Error(), "permission denied") {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "user is not a member of this tenant, or permission denied"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"accountID": accountToChange.ID, "tenantID": tenantID, "newRole": changeInfo.Role})
}

// handleAssociateProfileUpdate atualiza o perfil do associado no tenant atual.
func (s *Server) handleAssociateProfileUpdate(w http.ResponseWriter, r *http.Request) {
	var params npadmin.ProfileUpdateParams
	var lookup struct { Email string `json:"email" validate:"required,email"` }
	bodyBytes, err := io.ReadAll(r.Body)
	if err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": "cannot read request body"}); return }
	r.Body.Close()
	var bodyMap map[string]interface{}
	if err := json.Unmarshal(bodyBytes, &bodyMap); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid JSON body"}); return }
	if emailVal, ok := bodyMap["email"]; ok { if emailStr, ok := emailVal.(string); ok { lookup.Email = emailStr } }
	if err := s.validate.Struct(lookup); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": "email is required and must be valid"}); return }
	decoder := json.NewDecoder(bytes.NewReader(bodyBytes))
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(&params); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": fmt.Sprintf("error decoding profile fields: %v", err)}); return }
	if err := s.validate.Struct(params); err != nil { s.JSON(w, r, http.StatusUnprocessableEntity, err); return }
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok { s.LogError(r, errors.New("database transaction missing")); s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"}); return }
	accountToUpdate, err := s.AccountService.GetByEmail(tx, lookup.Email)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) { s.JSON(w, r, http.StatusNotFound, envelope{"message": "user not found"}); return }
		s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return
	}
	profileUpdateData := npadmin.ProfileUpdate{
		ID: accountToUpdate.ID, FirstName: params.FirstName, LastName: params.LastName, NIF: params.NIF, TLM: params.TLM,
	}
	err = s.ProfileService.UpdateProfile(tx, profileUpdateData)
	if err != nil {
		s.LogError(r, err)
		if strings.Contains(err.Error(), "no profile found") { s.JSON(w, r, http.StatusNotFound, envelope{"message": "profile not found for user in tenant"}); return }
		s.JSON(w, r, http.StatusInternalServerError, err); return
	}
	s.JSON(w, r, http.StatusOK, envelope{"profile": "updated", "id": profileUpdateData.ID})
}

// handleInsertPayment adiciona subscrição manual no tenant atual (via RLS em tx).
func (s *Server) handleInsertPayment(w http.ResponseWriter, r *http.Request) {
	var params npadmin.AddSubscriptionPayment
	if err := DecodeJSON(w, r, &params); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()}); return }
	if err := s.validate.Struct(params); err != nil { s.JSON(w, r, http.StatusUnprocessableEntity, err); return }
	startDate, err := time.Parse("2006-01-02", params.StartDate)
	if err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid start date format, use YYYY-MM-DD"}); return }
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok { s.LogError(r, errors.New("database transaction missing")); s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"}); return }
	err = s.addSubscriptionManually(tx, params.AccountID, int64(params.Period), params.Price, startDate) // Assumes function exists elsewhere
	if err != nil { s.LogError(r, err); s.JSON(w, r, http.StatusInternalServerError, err); return }
	s.JSON(w, r, http.StatusOK, envelope{"subscription": "added"})
}

// --- Funções de Extração CSV ---

// handleAssociatesExtract usa FilteredAssociates (RLS via tx)
func (s *Server) handleAssociatesExtract(w http.ResponseWriter, r *http.Request) {
	var params npadmin.FilterData
	qs := r.URL.Query()
	if err := s.decoder.Decode(&params, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}
	params.Page = 0
	params.PageSize = 0 // Extract all

	// Validação (mantém-se igual)
	tempParams := params
	tempParams.Page = 1
	tempParams.PageSize = 10
	if errVal := s.validate.Struct(tempParams); errVal != nil {
		var validationErrors validator.ValidationErrors
		if errors.As(errVal, &validationErrors) {
			filteredErrors := validationErrors[:0]
			for _, fieldErr := range validationErrors {
				if fieldErr.Field() != "Page" && fieldErr.Field() != "PageSize" {
					filteredErrors = append(filteredErrors, fieldErr)
				}
			}
			if len(filteredErrors) > 0 {
				s.JSON(w, r, http.StatusUnprocessableEntity, FormatValidationErrors(filteredErrors))
				return
			}
		} else {
			s.JSON(w, r, http.StatusUnprocessableEntity, errVal)
			return
		}
	}

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	// Agora recebemos 3 valores. Ignoramos o 'total' (o segundo valor) com um underscore '_'.
	associatesData, _, err := s.DataService.FilteredAssociates(tx, &params)

	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	buf := new(bytes.Buffer)
	writer := csv.NewWriter(buf)
	writer.Comma = ';'
	_ = writer.Write([]string{"Associates"})
	_ = writer.Write([]string{"Account ID", "Email", "Status ID", "NIF", "First Name", "Last Name", "Role", "Subscription Status", "Subscription End Date"})

	for _, associate := range associatesData {
		endDateStr := ""
		if associate.SubsEndDate.Valid {
			endDateStr = associate.SubsEndDate.Time.Format("2006-01-02 15:04:05")
		}
		row := []string{
			strconv.Itoa(associate.ID), associate.Email, strconv.Itoa(associate.Status),
			associate.NIF.String, associate.Name.String, associate.LastName.String,
			associate.MembershipRole,
			associate.SubscriptionStatus, endDateStr,
		}
		_ = writer.Write(row)
	}

	writer.Flush()
	if err := writer.Error(); err != nil {
		s.LogError(r, fmt.Errorf("error writing csv: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to generate csv"})
		return
	}
	setHeaders(w, "associates_export.csv", buf.Len())
	if _, err := io.Copy(w, buf); err != nil {
		s.LogError(r, fmt.Errorf("error copying csv buffer: %w", err))
	}
}

// handleAssociateDataExtract usa AssociateCompleteData (RLS via tx)
func (s *Server) handleAssociateDataExtract(w http.ResponseWriter, r *http.Request) {
	var associateFilter npadmin.AssociateFilter
	qs := r.URL.Query()
	if err := s.decoder.Decode(&associateFilter, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}
	// Validar
	if err := s.validate.Struct(associateFilter); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	// Buscar dados completos (reaproveita lógica com RLS)
	associateData, err := s.DataService.AssociateCompleteData(tx, associateFilter)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "associate profile not found in tenant"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	// Gerar CSV
	buf := new(bytes.Buffer)
	writer := csv.NewWriter(buf)
	writer.Comma = ';'
	
	// Cabeçalho Perfil
	_ = writer.Write([]string{"Profile"})
	_ = writer.Write([]string{"Account ID", "First Name", "Last Name", "NIF", "TLM", "Created At"})
	rowProfile := []string{
		strconv.FormatInt(associateData.Profile.ID, 10),
		associateData.Profile.FirstName.String,
		associateData.Profile.LastName.String,
		associateData.Profile.NIF.String,
		associateData.Profile.TLM.String,
		associateData.Profile.CreatedAt.Format("2006-01-02 15:04:05"),
	}
	_ = writer.Write(rowProfile)
	
	// Espaçamento
	_ = writer.Write([]string{""})
	
	// Cabeçalho Subscrições
	_ = writer.Write([]string{"Subscriptions History"})
	_ = writer.Write([]string{"Start Date", "End Date", "Price", "Invoice"})
	
	for _, subscription := range associateData.Subscriptions {
		invoiceStatus := "No"
		if subscription.Invoice.Valid && subscription.Invoice.String != "" {
			invoiceStatus = "Yes"
		}
		rowSubscription := []string{
			subscription.StartDate.Format("2006-01-02"),
			subscription.EndDate.Format("2006-01-02"),
			fmt.Sprintf("%.2f", subscription.Price),
			invoiceStatus,
		}
		_ = writer.Write(rowSubscription)
	}

	writer.Flush()
	if err := writer.Error(); err != nil {
		s.LogError(r, fmt.Errorf("error writing csv: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to generate csv"})
		return
	}
	
	setHeaders(w, "associate_export.csv", buf.Len())
	if _, err := io.Copy(w, buf); err != nil {
		s.LogError(r, fmt.Errorf("error copying csv buffer: %w", err))
	}
}

// setHeaders (função auxiliar para CSV - movida para cá ou mantida em download.go)
// Assumindo que foi mantida em download.go, esta pode ser removida.
/*
func setHeaders(w http.ResponseWriter, name string, length int) {
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, name))
	w.Header().Set("Content-Length", strconv.Itoa(length))
}
*/

// Import validator se FormatValidationErrors for usado aqui
// import "github.com/go-playground/validator/v10"

// Função auxiliar addSubscriptionManually (se não estiver em payment.go)
// func (s *Server) addSubscriptionManually(...) { ... }


// Helper function FormatValidationErrors (usually in error.go or http.go)
// Needed for handleAssociatesExtract validation part. Ensure it's accessible.
// If it's in error.go or http.go, no need to redefine here.
/*
func FormatValidationErrors(err error) map[string]string {
	//nolint
	errs := err.(validator.ValidationErrors)
	payload := make(map[string]string)
	for _, fe := range errs {
		payload[strings.ToLower(fe.Field())] = fe.ActualTag()
	}
	return payload
}
*/