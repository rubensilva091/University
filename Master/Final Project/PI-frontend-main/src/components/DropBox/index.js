import { Box, Typography, TextField, MenuItem } from '@mui/material'
import React, { useState } from 'react'
import { useTranslation } from 'react-i18next'

const DropBox = ({ prices, defaultValue, onChange }) => {
  const { t } = useTranslation()
  const [period, setPeriod] = useState(defaultValue)

  return (
    <Box>
      <Box mb="30px">
        <Typography
          sx={{
            mb: 1,
            fontSize: '1rem',
            fontWeight: 'Bold',
            color: 'rgba(0, 0, 0, 0.6)',
          }}
        >
          {t('payment.paymentPeriod')}:
        </Typography>
        <TextField
          value={period}
          onChange={(e) => {
            onChange((prevState) => ({
              ...prevState,
              period: e.target.value,
            }))
            setPeriod(e.target.value)
          }}
          id="payments"
          select
          sx={{
            width: '15rem',
          }}
        >
          {prices.prices.map((payment) => (
            <MenuItem key={payment.period} value={payment.period}>
              {payment.period} {t('paymentCategories.months')} - {payment.price}
              €
            </MenuItem>
          ))}
        </TextField>
      </Box>
    </Box>
  )
}
export default DropBox
