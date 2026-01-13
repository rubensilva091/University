import * as React from 'react'
import Radio from '@mui/material/Radio'
import RadioGroup from '@mui/material/RadioGroup'
import FormControlLabel from '@mui/material/FormControlLabel'
import FormControl from '@mui/material/FormControl'
import FormLabel from '@mui/material/FormLabel'
import Box from '@mui/material/Box'
import mbWay from '../../icon/mbWay.svg'
import mb from '../../icon/mb.svg'
import { useTranslation } from 'react-i18next'

export default function PaymentMethodSelector({ value, onChange }) {
  const { t } = useTranslation()
  return (
    <Box mb="20px">
      <FormControl focused={false}>
        <FormLabel
          id="methods-of-payment-label"
          sx={{ mb: 1, fontSize: '1rem', fontWeight: 'Bold' }}
        >
          {t('payment.methodOfPayment')}:
        </FormLabel>
        <RadioGroup
          name="methods-of-payment"
          aria-labelledby="methods-of-payment-label"
          value={value}
          onChange={(evt) => onChange(evt.target.value)}
        >
          <FormControlLabel
            control={<Radio size="small" />}
            label={<img src={mbWay} alt="image" width={50} />}
            value="mbway"
          />
          <FormControlLabel
            control={<Radio size="small" />}
            label={
              <Box
                component="img"
                src={mb}
                alt="image"
                width={40}
                marginLeft="-8px"
                marginBottom="-8px"
              />
            }
            value="mb"
          />
        </RadioGroup>
      </FormControl>
    </Box>
  )
}
