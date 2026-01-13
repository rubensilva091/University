import React from 'react'
import { Box, Typography } from '@mui/material'
import { LogotipoInvi } from '../../icon/LogotipoInvi.jsx'

export function NewPoweredBy() {
  return (
    <Box
      sx={{
        position: 'fixed',
        bottom: '1%',
        width: '100%',
        display: 'flex',
        flexDirection: 'row',
        alignItems: 'flex-end',
        justifyContent: 'center',
        gap: 1,
      }}
    >
      <Typography variant="caption">Powered by</Typography>
      <Box component="a" href="https://www.invisiblelab.dev/" height="24px">
        <LogotipoInvi height="100%" />
      </Box>
    </Box>
  )
}
