import * as React from 'react'
import CircularProgress from '@mui/material/CircularProgress'
import Box from '@mui/material/Box'

export default function CircularLoading() {
  return (
    <Box sx={{ display: 'flex' }}>
      <CircularProgress sx={{ color: 'rgb(254, 144, 43)' }} />
    </Box>
  )
}
