import React from 'react'
import { Box } from '@mui/material'

export const ImageBox = ({ imageUrl, sx, ImageProps, ...other }) => {
  return (
    <Box
      sx={{
        overflow: 'hidden',
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        ...sx,
      }}
      {...other}
    >
      <img
        src={imageUrl}
        style={{
          height: '100%',
          width: 'auto',
        }}
        loading="lazy"
        {...ImageProps}
      />
    </Box>
  )
}
