import React from 'react'
import { Button, CircularProgress, Typography, useTheme } from '@mui/material'
import { Link } from 'react-router-dom'

export const CustomButton = ({
  styleVariant,
  children,
  sx,
  to,
  selected,
  loading,
  typography,
  ...props
}) => {
  const { palette } = useTheme()
  const darkMode = palette.mode === 'dark'

  const renderLinkButton = () => {
    return (
      <Button component={Link} to={to} variant="text" sx={sx} {...props}>
        <Typography textTransform="none" {...typography}>
          {children}
        </Typography>
      </Button>
    )
  }

  const renderInvertedButton = () => {
    return (
      <Button
        variant="contained"
        sx={{
          px: 6,
          py: 2,
          '&:hover': {
            backgroundColor: darkMode
              ? palette.primary[800]
              : palette.primary[300],
          },
          backgroundColor: darkMode
            ? palette.primary[900]
            : palette.primary[200],
          color: darkMode ? palette.grey[200] : palette.grey[800],
          ...sx,
        }}
        component={to ? Link : undefined}
        to={to}
        {...props}
      >
        <Typography textTransform="none" {...typography}>
          {children}
        </Typography>
      </Button>
    )
  }

  const renderToggleButton = () => {
    let selectStyle
    if (selected)
      selectStyle = {
        '&:hover': {
          backgroundColor: darkMode
            ? palette.primary[900]
            : palette.primary[200],
        },
        backgroundColor: darkMode ? palette.primary[800] : palette.primary[200],
        color: darkMode ? palette.grey[200] : palette.grey[800],
      }
    else
      selectStyle = {
        '&:hover': {
          backgroundColor: darkMode
            ? palette.primary[900]
            : palette.primary[100],
        },
        color: darkMode ? palette.grey[200] : palette.grey[800],
      }

    return (
      <Button
        variant="text"
        sx={{
          ...selectStyle,
          ...sx,
        }}
        component={to ? Link : undefined}
        to={to}
        {...props}
      >
        {children}
      </Button>
    )
  }

  const renderDefaultButton = () => {
    return (
      <Button
        variant="contained"
        color="primary"
        sx={{
          px: 6,
          py: 2,
          ...sx,
        }}
        component={to ? Link : undefined}
        to={to}
        {...props}
        disabled={loading}
      >
        {loading ? (
          <CircularProgress size={24} color="grey" />
        ) : (
          <Typography textTransform="none" {...typography}>
            {children}
          </Typography>
        )}
      </Button>
    )
  }

  switch (styleVariant) {
    case 'link':
      return renderLinkButton()
    case 'inverted':
      return renderInvertedButton()
    case 'toggle':
      return renderToggleButton()
    default:
      return renderDefaultButton()
  }
}
