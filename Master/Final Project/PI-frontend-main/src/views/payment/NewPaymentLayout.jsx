import React from 'react'
import { Box } from '@mui/material'
import { NewPayment } from './NewPayment'

export const NewPaymentLayout = () => {
  return (
    <Box
      sx={{
        width: '100%',
        padding: { xs: 5, sm: 8, ml: 10, lg: 15 },
        display: 'flex',
        flexDirection: 'column',
        gap: 2,
      }}
    >
      <NewPayment />
    </Box>
  )
}
