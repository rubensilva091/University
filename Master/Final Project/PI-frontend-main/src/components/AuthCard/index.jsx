import React from 'react'
import { Box, Paper, Typography } from '@mui/material'
import { useTheme } from '@emotion/react'
import { ImageBox } from './ImageBox'
import { Language } from '../Language/index'

export const AuthCard = ({
  title,
  poster,
  logo,
  width,
  height,
  children,
  hideLogo,
}) => {
  const { palette } = useTheme()
  return (
    <Box
      sx={{
        px: { xs: 8, md: 15 },
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        minHeight: '100vh',
        height: '100%',
        width: '100vw',
        overflow: 'auto',
      }}
    >
      <Paper
        elevation={8}
        sx={{
          borderRadius: 8,
          height: height ? height : { xs: 'min-content', lg: '80vh' },
          overflow: 'hidden',
          display: 'flex',
        }}
      >
        <Box
          component="img"
          src={poster}
          sx={{
            display: { xs: 'none', lg: 'block' },
            height: '100%',
            width: 'auto',
          }}
          loading="lazy"
        />
        <Box
          sx={{
            p: { xs: 8, sm: 15 },
            display: 'flex',
            width: width,
            flexDirection: 'column',
            justifyContent: 'space-between',
          }}
        >
          <Box
            sx={{
              display: 'flex',
              justifyContent: 'end',
            }}
          >
            <Language />
          </Box>
          <Box
            sx={{
              display: 'flex',
              flexDirection: 'column',
              gap: { xs: 6, sm: 8 },
            }}
          >
            {logo ? (
              <Box
                sx={{
                  display: hideLogo ? { sm: 'flex', lg: 'none' } : 'flex',
                  maxHeight: '110px',
                }}
              >
                <img src={logo} alt="image" height="100%" width="100%" />
              </Box>
            ) : null}
            {title ? (
              <Typography variant="title" color="primary">
                {title}
              </Typography>
            ) : null}
            {children}
          </Box>
          <br />
        </Box>
      </Paper>
      <Box
        sx={{
          backgroundImage: `linear-gradient(to bottom right, ${palette.common.bgColor1}, ${palette.common.bgColor2}, ${palette.common.bgColor3}, ${palette.common.bgColor4})`,
          opacity: palette.common.bgOpacity,
          position: 'fixed',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          zIndex: -1,
        }}
      />
    </Box>
  )
}
