import React from 'react'
import { Box } from '@mui/material'
import { styled } from '@mui/material/styles'

export const CustomStepper = ({
  numberOfDots,
  current,
  dotProps,
  sx,
  ...other
}) => {
  const Dot = styled('div')(({ selected, theme }) => {
    return {
      padding: 6,
      borderRadius: '50%',
      backgroundColor:
        theme.palette.mode === 'light'
          ? selected
            ? theme.palette.primary.main
            : theme.palette.primary[200]
          : selected
            ? theme.palette.primary[400]
            : theme.palette.primary[800],
      ...dotProps,
    }
  })

  return (
    <Box
      sx={{
        width: '100%',
        p: 4,
        display: 'flex',
        flexDirection: 'row',
        gap: 3,
        justifyContent: 'center',
        ...sx,
      }}
      {...other}
    >
      {Array.from({ length: numberOfDots }, (_, index) => (
        <Dot key={index} selected={index === current} />
      ))}
    </Box>
  )
}
