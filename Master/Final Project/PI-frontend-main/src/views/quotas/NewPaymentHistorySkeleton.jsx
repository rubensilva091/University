import { Box, Skeleton } from '@mui/material'
import React from 'react'

export const NewPaymentHistorySkeleton = () => {
  return (
    <Box
      sx={{
        width: '100%',
        height: '100%',
      }}
    >
      <Skeleton sx={{ height: '60px', maxWidth: '500px', mb: 4 }} />
      <Skeleton />
      <Skeleton />
      <Skeleton />
      <Skeleton />
      <Skeleton />
      <Box
        sx={{
          width: '100%',
          display: 'flex',
          justifyContent: 'end',
        }}
      >
        <Skeleton sx={{ maxWidth: '500px', height: '40px' }} />
      </Box>
    </Box>
  )
}
