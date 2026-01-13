import React from 'react'
import Box from '@mui/material/Box'
import { Typography } from '@mui/material'
import { LogotipoInvi } from '../../icon/LogotipoInvi.jsx'

export function PoweredBy() {
  return (
    <Box
      sx={{
        position: 'fixed',
        bottom: '1%',
        right: '50%',
        left: '50%',
        width: 70,
      }}
    >
      <Typography sx={{ fontSize: '8px' }}>Powered by</Typography>
      <Box component="a" href="https://www.invisiblelab.dev/" height="24px">
        <LogotipoInvi height="100%" />
      </Box>
    </Box>
  )
}
