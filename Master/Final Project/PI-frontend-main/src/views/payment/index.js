import React, { useState, useEffect } from 'react'
import DropBox from '../../components/DropBox'
import PaymentMethodSelector from '../../components/PaymentMethodSelector'
import Box from '@mui/material/Box'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import CircularLoading from '../../components/CircularLoading'
import { Navigate, useNavigate } from 'react-router-dom'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { PayMB } from './PayMB'
import { PayMBWay } from './PayMBWay'
import Pusher from 'pusher-js'

export const PaymentView = () => {
  const [mbWayStatus, setMBWayStatus] = useState(false)
  const [paymentMethod, setPaymentMethod] = useState('')
  const [price, setPrice] = useState(null)
  const [payload, setPayload] = useState({
    jwt: null,
    period: null,
    tlm: null,
  })
  const { t } = useTranslation()
  const dispatch = useDispatch()
  const navigate = useNavigate()

  const loading = useSelector((state) => state.payment.loading)
  const failure = useSelector((state) => state.payment.failure)
  const prices = useSelector((state) => state.payment.prices)
  const mbWayPayment = useSelector((state) => state.payment.mbWayPayment)
  const mbPayment = useSelector((state) => state.payment.mbPayment)

  const id = useSelector((state) => state.global.id)

  useEffect(() => {
    const authTokens = JSON.parse(localStorage.getItem('authTokens'))
    setPayload((prevState) => ({
      ...prevState,
      jwt: authTokens,
    }))
    dispatch(actions.fetchPricesRequest(authTokens))
  }, [])

  useEffect(() => {
    const chosenPrice = prices?.find((p) => p.period === payload?.period)
    if (chosenPrice?.price) setPrice(chosenPrice.price)
  }, [payload, prices])

  useEffect(() => {
    if (mbWayPayment?.orderID) {
      const pusher = new Pusher(process.env.REACT_APP_PUSHER_KEY, {
        cluster: 'eu',
      })

      const channel = pusher.subscribe(id + '-mbway')
      channel.bind('mbway', function (data) {
        setMBWayStatus(true)
      })
    }
  }, [mbWayPayment])

  useEffect(() => {
    if (mbWayStatus) {
      return navigate('/quotas')
    }
  }, [mbWayStatus])

  const payMBWay = () => {
    dispatch(actions.newMbwaypayRequest(payload))
  }

  const payMB = () => {
    dispatch(actions.newMbpayRequest(payload))
  }

  if (loading) {
    return <CircularLoading />
  }
  if (failure) {
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
        {t('payment.title')}
      </Typography>
      <DropBox
        prices={{ prices }}
        onChange={setPayload}
        defaultValue={payload?.period || ''}
      />

      {payload.period !== null ? (
        <PaymentMethodSelector
          value={paymentMethod}
          onChange={setPaymentMethod}
        />
      ) : null}

      {paymentMethod === 'mbway' ? (
        <>
          <PayMBWay
            paymentFunction={payMBWay}
            phone={payload.tlm}
            amount={price}
            orderID={mbWayPayment?.orderID}
            onChange={setPayload}
          />
        </>
      ) : paymentMethod === 'mb' ? (
        <PayMB
          paymentFunction={payMB}
          reference={mbPayment?.reference}
          entity={mbPayment?.entity}
          amount={parseFloat(mbPayment?.amount).toFixed(2)}
          expiricyDate={mbPayment?.expiricyDate}
        />
      ) : null}
    </Box>
  )
}
