import React, { useState, useEffect } from 'react'
import { Link, Navigate } from 'react-router-dom'
import TableQuotas from '../../components/TableQuotas'
import Button from '@mui/material/Button'
import { Box, Typography } from '@mui/material'
import PaymentIcon from '@mui/icons-material/Payment'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import CircularLoading from '../../components/CircularLoading'
import { useTranslation } from 'react-i18next'
import StatusError from '../../components/StatusError'

const style = {
  background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
  borderRadius: 3,
  border: 0,
  color: 'white',
  height: 48,
  marginTop: '20px',
  padding: '0 30px',
  boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
}

export const QuotasView = () => {
  const { t } = useTranslation()

  const [, setPayload] = useState({
    jwt: null,
    page: null,
    pageSize: null,
  })

  const dispatch = useDispatch()
  const loading = useSelector((state) => state.quotas.loading)
  const failure = useSelector((state) => state.quotas.failure)
  const subscriptionHistory = useSelector(
    (state) => state.quotas.subscriptionHistory,
  )

  const status = useSelector((state) => state.quotas.status)

  useEffect(() => {
    const authTokens = JSON.parse(localStorage.getItem('authTokens'))
    setPayload((prevState) => ({
      ...prevState,
      jwt: authTokens,
    }))
    dispatch(
      actions.fetchQuotasRequest({
        jwt: authTokens,
      }),
    )
  }, [])

  if (loading) {
    return <CircularLoading />
  }

  if (failure) {
    if (status === 401) {
      return <Navigate to="/blank?status=401" />
    } else if (status) {
      const url = `/blank?status=${status}`
      return <Navigate to={url} />
    }
    return <StatusError status={status} />
  }

  return (
    <Box>
      <Typography
        color="#8984f0"
        fontWeight="fontWeightBold"
        fontSize="1.5rem"
        mb="20px"
      >
        {t('table.title')}
      </Typography>
      {subscriptionHistory && subscriptionHistory.length ? (
        <TableQuotas data={subscriptionHistory}></TableQuotas>
      ) : (
        <Typography color="black">{t('quotas.msgStatus')}</Typography>
      )}
      <Button
        size="medium"
        variant="contained"
        component={Link}
        to={'/payment'}
        style={style}
        startIcon={<PaymentIcon />}
      >
        {t('table.quotes.buttonPay')}
      </Button>
    </Box>
  )
}
