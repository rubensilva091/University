import DataAssociate from '../../components/AssociatesData'
import React, { useState, useEffect } from 'react'
import associateActions from '../associates/redux'
import actions from './redux'
import { useSelector, useDispatch } from 'react-redux'
import CircularLoading from '../../components/CircularLoading'
import Toast from '../../components/Toast'
import { useTranslation } from 'react-i18next'
import {
  Dialog,
  TablePagination,
  Button,
  DialogActions,
  DialogTitle,
  DialogContent,
  TextField,
  Box,
  InputAdornment,
  IconButton,
} from '@mui/material'
import AddBoxIcon from '@mui/icons-material/AddBox'
import { Navigate } from 'react-router-dom'

export const AdminView = () => {
  const labelDisplay = () => {
    return `Page ${payload.page}`
  }
  const addButtonStyle = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 35,
    width: 35,
    marginLeft: '10px',
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }
  const styleSimple = {
    background: 'white',
    borderRadius: 3,
    border: 0,
    color: '#8984f0',
    height: 48,
    // margin: "30px  20px",
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }
  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,
    marginLeft: '10px',
    // margin: "30px  20px",
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }
  const [payload, setPayload] = useState({
    jwt: null,
    email: null,
    role: null,
    firstName: null,
    lastName: null,
    nif: null,
    tlm: null,
    page: 1,
    pageSize: 4,
  })
  const [open, setOpen] = useState(false)
  const [startDate, setStartDate] = useState('')
  const [price, setPrice] = useState(0.0)
  const [period, setPeriod] = useState(6)
  const dispatch = useDispatch()
  const loading = useSelector((state) => state.associateProfile.loading)
  const errorStatus = useSelector((state) => state.associateProfile.errorStatus)
  const associateData = useSelector((state) => state.associates.associateData)
  const fetchErrorStatus = useSelector((state) => state.associates.errorStatus)

  const profileEditFail = useSelector((state) => state.associateProfile.failure)
  const success = useSelector((state) => state.associateProfile.success)

  const authTokens = JSON.parse(localStorage.getItem('authTokens'))
  const queryParameters = new URLSearchParams(window.location.search)
  const email = queryParameters.get('email')
  const { t } = useTranslation()
  const successEdit = t('toasts.profileTrue')
  const failedEdit = t('toasts.profileFail')
  const successSub = t('toasts.successSub')
  useEffect(() => {
    if (email) {
      setPayload({
        jwt: authTokens,
        email,
        page: payload.page,
        pageSize: payload.pageSize,
      })
    }
  }, [])
  useEffect(() => {
    if (email) {
      dispatch(
        associateActions.associateDataRequest({
          jwt: authTokens,
          email,
          page: payload.page,
          pageSize: payload.pageSize,
        }),
      )
    }
  }, [payload.page, payload.pageSize])

  useEffect(() => {
    if (payload.firstName || payload.lastName || payload.nif || payload.tlm) {
      dispatch(
        actions.updateProfileRequest({
          jwt: payload.jwt,
          email: payload.email,
          firstName: payload.firstName.toLowerCase(),
          lastName: payload.lastName.toLowerCase(),
          nif: payload.nif,
          tlm: payload.tlm,
        }),
      )
    }
    if (payload.role) {
      dispatch(
        actions.changeRoleRequest({
          jwt: payload.jwt,
          email: payload.email,
        }),
      )
      payload.role = null
    }
  }, [
    payload.firstName,
    payload.lastName,
    payload.nif,
    payload.tlm,
    payload.role,
  ])

  useEffect(() => {
    if (success) {
      dispatch(
        associateActions.associateDataRequest({
          jwt: authTokens,
          email,
          page: payload.page,
          pageSize: payload.pageSize,
        }),
      )
    }
  }, [success])

  const handleStartManualSub = () => {
    setOpen(true)
  }

  const handleStopManualSub = () => {
    setOpen(false)
  }

  const handleSubmitSubscription = () => {
    dispatch(
      actions.manualSubscriptionRequest({
        jwt: payload.jwt,
        accountId: associateData.profile.ID,
        period: parseInt(period),
        startDate,
        price: parseFloat(price),
      }),
    )
    setOpen(false)
  }

  const handleSubmitInvoice = (subscriptionID, subscriptionInvoice) => {
    dispatch(
      actions.uploadInvoiceRequest({
        jwt: payload.jwt,
        id: associateData.profile.ID,
        subscriptionId: subscriptionID,
        invoiceUrl: subscriptionInvoice,
      }),
    )
  }
  const handleChangePage = (event, newPage) => {
    if (newPage === 0) {
      newPage = 1
    }
    setPayload((prevState) => ({
      ...prevState,
      page: newPage,
    }))
  }

  const handleDownloadAssociate = () => {
    console.log('Downloading ', email)
    dispatch(
      associateActions.downloadAssociateRequest({
        jwt: payload.jwt,
        email,
      }),
    )
  }

  if (loading) {
    return <CircularLoading />
  }

  if (fetchErrorStatus === 401 || errorStatus === 401) {
    return <Navigate to="/blank?status=401" />
  }

  return (
    <>
      {associateData ? (
        <Box>
          <DataAssociate
            associateData={associateData}
            email={email}
            editProfile={setPayload}
            handleSubmitInvoice={handleSubmitInvoice}
            extractAssociate={handleDownloadAssociate}
          />
          <Box
            sx={{
              display: 'flex',
              flexDirection: 'row',
              justifyContent: 'space-between',
            }}
          >
            <IconButton
              aria-label="add"
              height="10"
              variant="contained"
              style={addButtonStyle}
              onClick={handleStartManualSub}
            >
              <AddBoxIcon />
            </IconButton>
            <TablePagination
              rowsPerPageOptions={[4, 10, 25, 50]}
              component="div"
              count={-1}
              rowsPerPage={payload.pageSize}
              page={payload.page}
              onPageChange={handleChangePage}
              onRowsPerPageChange={(e) =>
                setPayload((prevState) => ({
                  ...prevState,
                  pageSize: e.target.value,
                  page: 1,
                }))
              }
              labelDisplayedRows={labelDisplay}
            />
          </Box>
        </Box>
      ) : null}
      {open ? (
        <Dialog
          open={open}
          onClose={handleStopManualSub}
          aria-labelledby="alert-dialog-title"
          aria-describedby="alert-dialog-description"
          sx={{ height: '80vh' }}
        >
          <DialogTitle>{t('paymentSettings.manualSubscription')}</DialogTitle>
          <DialogContent>
            <Box
              component="form"
              sx={{
                display: 'flex',
                flexDirection: 'column',
                m: 'auto',
                width: 'fit-content',
                height: 'fit-content',
              }}
            >
              <TextField
                id="ID"
                label={t('associateData.associate')}
                type="ID"
                fullWidth
                variant="outlined"
                InputProps={{
                  readOnly: true,
                }}
                sx={{
                  mb: 2,
                  mt: 2,
                }}
                value={associateData.profile.ID}
              />
              <TextField
                id="outlined-basic"
                label={t('associatesTable.period')}
                variant="outlined"
                fullWidth
                helperText={t('associatesTable.inMonths')}
                sx={{
                  mb: 2,
                  mt: 2,
                }}
                value={period}
                onChange={(e) => setPeriod(e.target.value)}
              />
              <TextField
                name="startDate"
                label={t('table.quotes.date')}
                InputLabelProps={{ shrink: true }}
                type="date"
                onChange={(e) => setStartDate(e.target.value)}
                value={startDate}
              />
              <TextField
                id="Price"
                label={t('table.quotes.value')}
                type="number"
                inputProps={{
                  min: 0.01,
                  step: '0.01',
                }}
                fullWidth
                variant="outlined"
                InputProps={{
                  startAdornment: (
                    <InputAdornment position="start">€</InputAdornment>
                  ),
                }}
                sx={{
                  mb: 2,
                  mt: 2,
                }}
                value={price}
                onChange={(e) => setPrice(e.target.value)}
              />
              <DialogActions>
                <Button
                  variant="contained"
                  size="small "
                  style={styleSimple}
                  onClick={() => handleSubmitSubscription()}
                >
                  {t('table.quotes.submit')}
                </Button>
                <Button
                  onClick={handleStopManualSub}
                  size="small "
                  style={style}
                  variant="contained"
                >
                  {t('table.quotes.cancel')}
                </Button>
              </DialogActions>
            </Box>
          </DialogContent>
        </Dialog>
      ) : null}
      {success ? <Toast status={true} message={successEdit}></Toast> : null}
      {success.newSub ? (
        <Toast status={true} message={successSub}></Toast>
      ) : null}
      {profileEditFail ? (
        <Toast status={false} message={failedEdit}></Toast>
      ) : null}
    </>
  )
}
