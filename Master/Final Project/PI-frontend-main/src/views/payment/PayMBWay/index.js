import React from 'react'
import Box from '@mui/material/Box'
import InputAdornment from '@mui/material/InputAdornment'
import CardMbWay from '../../../components/CardMbWay'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import Button from '@mui/material/Button'
import TextField from '@mui/material/TextField'
import SmartphoneIcon from '@mui/icons-material/Smartphone'

export const PayMBWay = ({
  paymentFunction,
  phone,
  amount,
  onChange,
  orderID,
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

  const handleChange = (e) => {
    const regex = /^[0-9\b]+$/
    if (e.target.value === '' || regex.test(e.target.value)) {
      onChange((prevState) => ({
        ...prevState,
        tlm: e.target.value.split(' ').join(''),
      }))
    }
  }

  return (
    <>
      {orderID ? (
        <CardMbWay phone={phone} amount={amount} orderID={orderID} />
      ) : (
        <>
          <Box component="form" autoComplete="off" mb="10px">
            <Typography
              sx={{
                mb: 1,
                fontSize: '1rem',
                fontWeight: 'Bold',
                color: 'rgba(0, 0, 0, 0.6)',
              }}
            >
              {t('payment.subtitleToNumberPhone')}
            </Typography>
            <TextField
              id="inputNumber"
              variant="outlined"
              value={phone || ''}
              onChange={(e) => handleChange(e)}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <SmartphoneIcon fontSize="small" />
                  </InputAdornment>
                ),
              }}
            />
          </Box>
          <Button
            size="medium"
            variant="contained"
            style={style}
            onClick={paymentFunction}
          >
            {t('payment.buttonMbway')}
          </Button>
        </>
      )}
    </>
  )
}
