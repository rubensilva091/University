import React from 'react'
import Box from '@mui/material/Box'
import { Typography, useTheme } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { QRCodeSVG } from 'qrcode.react'

const QRCodeLabelText = ({ label, text }) => {
  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'row',
        gap: 2,
      }}
    >
      <Typography sx={{ fontSize: '0.9rem' }}>{label}:</Typography>
      <Typography sx={{ fontWeight: 600, fontSize: '0.9rem' }}>
        {text}
      </Typography>
    </Box>
  )
}

export const NewQRCode = ({ name, id }) => {
  const { t } = useTranslation()
  const { palette } = useTheme()
  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        gap: 6,
        borderRadius: '20px',
        p: 10,
        border: '2px solid',
        borderColor: palette.grey[600],
      }}
    >
      <QRCodeSVG
        value={`id:${id};name:${name}`}
        level="H"
        size={195}
        fgColor={palette.grey[800]}
        imageSettings={{
          src: '/favicon.ico',
          height: 50,
          width: 50,
          excavate: true,
        }}
      />
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
        }}
      >
        <QRCodeLabelText label={t('dashboard.id')} text={id} />
        <QRCodeLabelText label={t('dashboard.name')} text={name} />
      </Box>
    </Box>
  )
}
