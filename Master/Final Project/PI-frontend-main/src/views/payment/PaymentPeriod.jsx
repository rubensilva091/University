import React from 'react'
import { Box, CircularProgress, useTheme } from '@mui/material'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import { useTranslation } from 'react-i18next'
import { CustomSelect } from '../../components/CustomInput/CustomSelect'
import Typography from '@mui/material/Typography'

export const PaymentPeriod = () => {
  const { t } = useTranslation()
  const { palette } = useTheme()

  const loading = useSelector((state) => state.payment.loading)
  const error = useSelector((state) => state.payment.error)
  const option = useSelector((state) => state.payment.option)
  const prices = useSelector((state) => state.payment.prices)
  const dispatch = useDispatch()

  if (loading && prices.length === 0) {
    return (
      <Box
        sx={{
          width: '100%',
          height: '100%',
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        <CircularProgress color="secondary" />
      </Box>
    )
  } else {
    return (
      <>
        <Box
          sx={{
            display: 'flex',
            flexDirection: 'column',
            gap: 2,
          }}
        >
          <Typography sx={{ fontWeight: 700, color: palette.common.darkGrey }}>
            {t('payment.paymentPeriod')}
          </Typography>
          <CustomSelect
            options={prices}
            value={option}
            sx={{ bgcolor: 'white' }}
            onValueChange={(value) => {
              dispatch(
                actions.setField({
                  option: value,
                  error: {
                    ...error,
                    option: '',
                  },
                }),
              )
            }}
            renderValue={(value) =>
              `${value.period} ${t('paymentCategories.months')}`
            }
            renderOption={(value) =>
              `${value.period} ${t('paymentCategories.months')} - ${value.price}€`
            }
            error={error.option !== ''}
            helperText={t(error.option)}
          />
        </Box>
        <Box
          sx={{
            display: 'flex',
            alignItems: 'center',
            gap: 4,
            color: palette.common.darkGrey,
            width: '100%',
            justifyContent: 'end',
          }}
        >
          <Typography sx={{ fontWeight: 700 }}>
            {t('payment.total')}:
          </Typography>
          <Box
            sx={{
              bgcolor: 'white',
              paddingInline: 4,
              paddingBlock: 1,
              border: 2,
              borderColor: palette.primary[200],
              borderRadius: 2,
            }}
          >
            <Typography sx={{ fontWeight: 500 }}>{option.price}€</Typography>
          </Box>
        </Box>
      </>
    )
  }
}
