import { Box } from '@mui/material'
import React from 'react'
import { NewStatusCard } from '../StatusCard/NewStatusCard'

export const NewStatusError = ({ status }) => {
  return (
    <Box
      sx={{
        width: '100%',
        height: '100%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <NewStatusCard status={status} />
    </Box>
  )
}
