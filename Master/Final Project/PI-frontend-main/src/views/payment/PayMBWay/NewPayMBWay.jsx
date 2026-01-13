import React, { useEffect, useState } from 'react'
import {
  Typography,
  Box,
  InputAdornment,
  CircularProgress,
} from '@mui/material'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import { Phone } from '@mui/icons-material'
import actions from '../redux'
import { CustomButton } from '../../../components/CustomButton'
import { NumberTextField } from '../../../components/CustomInput/NumberTextField'
import { NewCardMBWay } from '../../../components/CardMbWay/NewCardMBWay'

const MINUTES = 5

export const NewPayMBWay = () => {
  const { t } = useTranslation()

  const option = useSelector((state) => state.payment.option)
  const loading = useSelector((state) => state.payment.loading)
  const mbWayStatus = useSelector((state) => state.payment.mbWayStatus)
  const time = useSelector((state) => state.payment.time)
  const expireTime = useSelector((state) => state.payment.expireTime)
  const tlm = useSelector((state) => state.payment.tlm)
  const jwt = useSelector((state) => state.payment.jwt)
  const error = useSelector((state) => state.payment.error)
  const mbWayPayment = useSelector((state) => state.payment.mbWayPayment)
  const dispatch = useDispatch()

  const [modalIsOpen, setModalIsOpen] = useState(false)

  const payMBWay = () => {
    if (option === undefined)
      dispatch(
        actions.setField({
          error: {
            ...error,
            option: 'validation.required',
          },
        }),
      )
    else if (tlm.length !== 9)
      dispatch(
        actions.setField({
          error: {
            ...error,
            tlm: 'validation.phoneNumber',
          },
        }),
      )
    else {
      setModalIsOpen(true)
      dispatch(
        actions.newMbwaypayRequest({
          jwt: jwt,
          period: option.period,
          tlm: tlm,
          expireTime: 60 * 1000 * MINUTES + new Date().getTime(),
        }),
      )
    }
  }

  useEffect(() => {
    const interval = setInterval(() => {
      if (mbWayPayment && mbWayPayment.orderID) {
        const newTime = new Date().getTime()
        if (newTime < expireTime) dispatch(actions.setField({ time: newTime }))
        else dispatch(actions.reset())
      }
    }, 1000)
    return () => clearInterval(interval)
  }, [mbWayPayment])

  useEffect(() => {
    if (mbWayStatus) {
      dispatch(actions.reset())
    }
  }, [mbWayPayment])

  const getTime = () => {
    if (expireTime === undefined) return t('paymentReference.expired')
    const totalSeconds = Math.floor((expireTime - time) / 1000)
    const minutes = String(Math.floor(totalSeconds / 60)).padStart(2, '0')
    const seconds = String(totalSeconds % 60).padStart(2, '0')
    return `${minutes}:${seconds}`
  }

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        padding: 4,
        gap: 2,
      }}
    >
      <Typography>{t('payment.subtitleToNumberPhone')}</Typography>
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'row',
          gap: 4,
          alignItems: 'start',
          width: '100%',
          maxWidth: 'fit-content',
        }}
      >
        <NumberTextField
          value={tlm}
          allowNegative={false}
          decimalScale={0}
          onValueChange={(value) =>
            dispatch(
              actions.setField({
                tlm: value,
                error: {
                  ...error,
                  tlm: '',
                },
              }),
            )
          }
          inputProps={{
            style: { paddingInline: '5px', height: '20px' },
          }}
          InputProps={{
            startAdornment: (
              <InputAdornment position="start">
                <Phone />
              </InputAdornment>
            ),
          }}
          sx={{ maxWidth: '250px', minWidth: '100px' }}
          error={error.tlm !== ''}
          helperText={t(error.tlm)}
        />
        {loading ? (
          <Box
            sx={{
              height: '40px',
              paddingInline: 6,
              display: 'flex',
              alignItems: 'center',
            }}
          >
            <CircularProgress color="secondary" size={30} />
          </Box>
        ) : time !== undefined ? (
          <CustomButton onClick={() => setModalIsOpen(true)}>
            {t('payment.buttonMbwayOpen')}
          </CustomButton>
        ) : (
          <CustomButton onClick={payMBWay}>
            {t('payment.buttonMbway')}
          </CustomButton>
        )}
      </Box>
      {mbWayPayment && mbWayPayment.orderID && (
        <NewCardMBWay
          isOpen={modalIsOpen}
          close={() => setModalIsOpen(false)}
          phone={tlm}
          amount={option.price}
          orderID={mbWayPayment.orderID}
          time={getTime(time)}
        />
      )}
    </Box>
  )
}
