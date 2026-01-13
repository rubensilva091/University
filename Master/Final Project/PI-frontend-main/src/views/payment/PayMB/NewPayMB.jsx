import React, { useState } from 'react'
import Box from '@mui/material/Box'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import actions from '../redux'
import { CustomButton } from '../../../components/CustomButton'
import { NewCardMB } from '../../../components/CardMB/NewCardMB'

export const NewPayMB = () => {
  const { t } = useTranslation()

  const option = useSelector((state) => state.payment.option)
  const tlm = useSelector((state) => state.payment.tlm)
  const jwt = useSelector((state) => state.payment.jwt)
  const mbPayment = useSelector((state) => state.payment.mbPayment)
  const dispatch = useDispatch()

  const [modalIsOpen, setModalIsOpen] = useState(false)

  const payMB = () => {
    if (option === undefined)
      dispatch(actions.setField({ error: { option: 'validation.required' } }))
    else if (mbPayment && mbPayment.reference) {
      setModalIsOpen(!modalIsOpen)
    } else {
      setModalIsOpen(!modalIsOpen)
      dispatch(
        actions.newMbpayRequest({
          jwt: jwt,
          period: option.period,
          tlm: tlm,
        }),
      )
    }
  }

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        padding: 4,
        gap: 2,
      }}
    >
      <Typography>{t('payment.subtitleMbButton')}</Typography>
      <CustomButton variant="contained" onClick={payMB}>
        {mbPayment && mbPayment.reference
          ? t('payment.buttonMbOpen')
          : t('payment.buttonMb')}
      </CustomButton>
      {mbPayment && mbPayment.reference && (
        <NewCardMB
          isOpen={modalIsOpen}
          close={() => setModalIsOpen(false)}
          reference={mbPayment.reference}
          entity={mbPayment.entity}
          amount={parseFloat(mbPayment.amount).toFixed(2)}
          expiricyDate={mbPayment.expiricyDate}
        />
      )}
    </Box>
  )
}
