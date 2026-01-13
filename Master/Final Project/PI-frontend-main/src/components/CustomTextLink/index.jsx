import React from 'react'
import { Box, Typography } from '@mui/material'
import { CustomButton } from '../CustomButton'

export const CustomTextLink = ({
  sx,
  TypographyProps,
  CustomButtonProps,
  to,
  questionText,
  linkText,
  ...props
}) => {
  return (
    <Box
      sx={{
        display: 'flex',
        gap: 2,
        alignItems: 'center',
        ...sx,
      }}
      {...props}
    >
      {questionText ? (
        <Typography variant="body1" {...TypographyProps}>
          {questionText}
        </Typography>
      ) : null}
      <CustomButton styleVariant="link" to={to} {...CustomButtonProps}>
        {linkText}
      </CustomButton>
    </Box>
  )
}
