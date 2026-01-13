import React from 'react'
import { Divider, Typography } from '@mui/material'

export const CustomSeparator = ({
  children,
  dividerProps,
  typographyProps,
}) => {
  return (
    <Divider
      variant="middle"
      sx={{
        my: children ? 0 : 2,
        '&::before, &::after': { borderWidth: '2px' },
      }}
      {...dividerProps}
    >
      {children ? (
        <Typography fontWeight={600} fontSize="" {...typographyProps}>
          {children}
        </Typography>
      ) : null}
    </Divider>
  )
}
