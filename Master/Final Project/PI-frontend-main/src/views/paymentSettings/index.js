import React, { useState, useEffect } from 'react'
import { useSelector, useDispatch } from 'react-redux'
import Box from '@mui/material/Box'
import { PaymentCategories } from '../../components/PaymentCategories'
import TablePayments from '../../components/TablePayments'
import { Typography } from '@mui/material'
import actions from './redux'
import Toast from '../../components/Toast'
import { useTranslation } from 'react-i18next'
import { Navigate } from 'react-router-dom'

export const PaymentSettings = () => {
  const [authTokens, setAuthTokens] = useState(undefined)
  const dispatch = useDispatch()
  const { t } = useTranslation()

  const prices = useSelector((state) => state.paymentSettings.prices)
  const addPriceSuccess = useSelector(
    (state) => state.paymentSettings.addPriceSuccess,
  )
  const addPriceFailed = useSelector(
    (state) => state.paymentSettings.addPriceFailed,
  )

  const deletePriceSuccess = useSelector(
    (state) => state.paymentSettings.deletePriceSuccess,
  )
  const deletePriceFailed = useSelector(
    (state) => state.paymentSettings.deletePriceFailed,
  )

  const errorStatus = useSelector((state) => state.paymentSettings.errorStatus)

  useEffect(() => {
    const authTokens = JSON.parse(localStorage.getItem('authTokens'))
    setAuthTokens(authTokens)
    if (authTokens) {
      dispatch(actions.fetchAllPricesRequest(authTokens))
    }
  }, [authTokens])

  useEffect(() => {
    if (authTokens && (addPriceSuccess || deletePriceSuccess)) {
      dispatch(actions.fetchAllPricesRequest(authTokens))
    }
  }, [addPriceSuccess, deletePriceSuccess])

  const addPrice = (category, price, period) => {
    dispatch(
      actions.addPriceRequest({
        jwt: authTokens,
        category,
        price,
        period,
      }),
    )
  }

  const deletePrice = (category, period) => {
    dispatch(
      actions.deletePriceRequest({
        jwt: authTokens,
        category,
        period,
      }),
    )
  }

  if (errorStatus === 401) {
    return <Navigate to="/blank?status=401" />
  }

  return (
    <Box>
      <Typography
        color="#8984f0"
        fontWeight="fontWeightBold"
        fontSize="1.5rem"
        mb="20px"
      >
        {t('paymentSettings.title')}
      </Typography>
      <TablePayments
        paymentOptions={prices}
        deleteOption={deletePrice}
      ></TablePayments>
      <Typography
        color="#8984f0"
        fontWeight="fontWeightBold"
        fontSize="1.5rem"
        mb="20px"
        paddingTop={5}
      >
        {t('paymentSettings.subtitle')}
      </Typography>
      <PaymentCategories
        submitCategory={addPrice}
        categories={[...new Set(prices?.map(({ category }) => category))]}
      />
      {addPriceSuccess ? (
        <Toast status={true} message={t('toasts.addPrice')}></Toast>
      ) : null}
      {addPriceFailed ? (
        <Toast status={false} message={t('toasts.addPriceFail')}></Toast>
      ) : null}

      {deletePriceSuccess ? (
        <Toast status={true} message={t('toasts.deletePrice')}></Toast>
      ) : null}
      {deletePriceFailed ? (
        <Toast status={false} message={t('toasts.deletePriceFail')}></Toast>
      ) : null}
    </Box>
  )
}
