import React, { useEffect } from 'react'
import { Box, useTheme } from '@mui/material'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import { PaymentPeriod } from './PaymentPeriod'
import { PaymentMethod } from './PaymentMethod'

export const NewPayment = () => {
  const { palette } = useTheme()

  const loading = useSelector((state) => state.payment.loading)
  const prices = useSelector((state) => state.payment.prices)
  const option = useSelector((state) => state.payment.option)
  const dispatch = useDispatch()

  useEffect(() => {
    const authTokens = JSON.parse(localStorage.getItem('authTokens'))
    dispatch(actions.setField({ jwt: authTokens }))
    dispatch(actions.fetchPricesRequest(authTokens))
  }, [])

  return (
    <Box
      sx={{
        width: '100%',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      {(prices.length === 0 && loading) ||
      (prices.length !== 0 && option !== undefined) ? (
        <Box
          sx={{
            width: '100%',
            maxWidth: '1000px',
            display: 'flex',
            flexDirection: { xs: 'column', lg: 'row' },
            border: 2,
            borderColor: palette.primary.main,
            borderRadius: 4,
            overflow: 'hidden',
          }}
        >
          <Box
            sx={{
              width: { xs: '100%', lg: '40%' },
              minWidth: 'max-content',
              paddingBlock: 5,
              paddingInline: { xs: 10, lg: 15 },
              display: 'flex',
              flexDirection: 'column',
              gap: 2,
              justifyContent: 'space-between',
              bgcolor: palette.primary[50],
            }}
          >
            <PaymentPeriod />
          </Box>
          <Box
            sx={{
              border: 1,
              borderColor: palette.primary.main,
            }}
          ></Box>
          <Box
            sx={{
              width: { xs: '100%', lg: '60%' },
              paddingBlock: 0,
              paddingInline: { xs: 5, sm: 10, lg: 12 },
              display: 'flex',
              flexDirection: 'column',
            }}
          >
            <PaymentMethod />
          </Box>
        </Box>
      ) : null}
    </Box>
  )
}
