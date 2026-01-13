import React from 'react'
import Box from '@mui/material/Box'
import CardMB from '../../../components/CardMB'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import Button from '@mui/material/Button'

export const PayMB = ({
  paymentFunction,
  reference,
  entity,
  amount,
  expiricyDate,
}) => {
  const { t } = useTranslation()
  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  return (
    <>
      {reference ? (
        <CardMB
          reference={reference}
          entity={entity}
          amount={amount}
          expiricyDate={expiricyDate}
        />
      ) : (
        <>
          <Box component="form" autoComplete="off" mb="10px">
            <Typography
              sx={{
                mb: 1,
                fontSize: '1rem',
                color: 'rgba(0, 0, 0, 0.6)',
              }}
            >
              {t('payment.subtitleMbButton')}
            </Typography>
          </Box>
          <Button
            size="medium"
            variant="contained"
            style={style}
            onClick={paymentFunction}
          >
            {t('payment.buttonMb')}
          </Button>
        </>
      )}
    </>
  )
}
