import { Box, TablePagination, Typography, Button } from '@mui/material'
import TextField from '@mui/material/TextField'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import { Navigate } from 'react-router-dom'
import StatusError from '../../components/StatusError'
import TableAssociates from '../../components/TableAssociates'
import { capitalizeWords } from '../../utils/utils'
import actions from './redux'

export function Associates() {
  const addButtonStyle = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 30,
    width: 130,
    marginTop: '4px',
  }
  const [payload, setPayload] = useState({
    jwt: null,
    page: 1,
    pageSize: 5,
    name: null,
    status: null,
    subStatus: null,
  })
  const [disableAccount, setDisableAccount] = useState('')
  const [verifyAccount, setVerifyAccount] = useState('')
  const { t } = useTranslation()

  const dispatch = useDispatch()
  const failure = useSelector((state) => state.associates.failure)
  const status = useSelector((state) => state.associates.errorStatus)

  const associates = useSelector((state) => state.associates.associates)

  useEffect(() => {
    const authTokens = JSON.parse(localStorage.getItem('authTokens'))
    setPayload((prevState) => ({
      ...prevState,
      jwt: authTokens,
    }))
    dispatch(
      actions.fetchAssociatesRequest({
        jwt: authTokens,
        page: payload.page,
        pageSize: payload.pageSize,
        name: payload.name,
        status: payload.status,
        subStatus: payload.subStatus,
      }),
    )
  }, [])
  useEffect(() => {
    if (payload.jwt) {
      dispatch(
        actions.fetchAssociatesRequest({
          jwt: payload.jwt,
          page: payload.page,
          pageSize: payload.pageSize,
          name: payload.name,
          status: payload.status,
          subStatus: payload.subStatus,
        }),
      )
    }
  }, [payload])
  useEffect(() => {
    if (payload.jwt && disableAccount !== '') {
      dispatch(
        actions.disableAssociateRequest({
          jwt: payload.jwt,
          email: disableAccount,
        }),
      )
      setPayload((prevState) => ({
        ...prevState,
        refresh: !prevState,
      }))
    }
  }, [disableAccount])
  useEffect(() => {
    if (payload.jwt && verifyAccount !== '') {
      dispatch(
        actions.verifyAssociateRequest({
          jwt: payload.jwt,
          email: verifyAccount,
        }),
      )
      setPayload((prevState) => ({
        ...prevState,
        refresh: !prevState,
      }))
    }
  }, [verifyAccount])

  const labelDisplay = () => {
    return `Page ${payload.page}`
  }

  const handleChangePage = (event, newPage) => {
    setPayload((prevState) => ({
      ...prevState,
      page: newPage,
    }))
  }

  const handleSearch = (value) => {
    if (value.length > 0) {
      setPayload((prevState) => ({
        ...prevState,
        page: 1,
        name: value,
      }))
    } else {
      setPayload((prevState) => ({
        ...prevState,
        page: 1,
        name: null,
      }))
    }
  }

  const handleDownloadAssociate = (account) => {
    console.log('Downloading ', account)
    dispatch(
      actions.downloadAssociateRequest({
        jwt: payload.jwt,
        email: account,
      }),
    )
  }

  const handleDownloadAssociates = () => {
    console.log('Downloading all associates')
    dispatch(
      actions.downloadAssociatesRequest({
        jwt: payload.jwt,
      }),
    )
  }

  if (failure) {
    if (status) {
      const url = `/blank?status=${status}`
      return <Navigate to={url} />
    }
    return <StatusError statusCode={status} />
  }
  return (
    <Box>
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
        }}
      >
        <Typography
          color="#8984f0"
          fontWeight="fontWeightBold"
          fontSize="1.5rem"
          mb="20px"
        >
          {t('associatesTable.title')}
        </Typography>
        <Button
          variant="contained"
          onClick={handleDownloadAssociates}
          style={addButtonStyle}
          size="small"
        >
          {t('associatesTable.exportAll')}
        </Button>
      </Box>
      <TextField
        label={t('associatesTable.searchBar')}
        fullWidth
        value={
          payload.last_name
            ? capitalizeWords(payload.first_name + ' ' + payload.last_name)
            : payload.first_name
        }
        onChange={(e) => handleSearch(e.target.value)}
        variant="outlined"
        InputProps={{ sx: { borderRadius: 1, marginBottom: 4 } }}
      ></TextField>
      <TableAssociates
        data={associates}
        status={payload.status}
        subStatus={payload.subStatus}
        onChange={setPayload}
        onDisable={setDisableAccount}
        onVerify={setVerifyAccount}
        handleDownloadAssociate={handleDownloadAssociate}
      ></TableAssociates>
      <TablePagination
        rowsPerPageOptions={[5, 10, 25, 50]}
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
      {associates && associates.length ? null : (
        <Typography color="black">
          {t('associatesTable.noAssociates')}
        </Typography>
      )}
    </Box>
  )
}
