import React, { useState } from 'react'
import Box from '@mui/material/Box'
import Typography from '@mui/material/Typography'
import { IconButton, Tooltip, useTheme } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { ContentCopy } from '@mui/icons-material'

export const ClipboardText = ({ text }) => {
  const { palette } = useTheme()
  const { t } = useTranslation()

  const [copied, setCopied] = useState(false)

  const handleClick = () => {
    setCopied(true)
    navigator.clipboard.writeText(text)
  }

  return (
    <Box
      sx={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        borderRadius: 4,
        p: 1,
        pl: 4,
        ':hover': {
          bgcolor: palette.grey[100],
        },
      }}
    >
      <Typography>{text}</Typography>
      <Tooltip
        title={
          copied
            ? t('paymentReference.copied')
            : t('paymentReference.clipboard')
        }
        onBlur={() => setCopied(false)}
        arrow
        placement="top"
        disableInteractive
      >
        <IconButton
          size="small"
          onClick={handleClick}
          sx={{
            borderRadius: 3,
            ':hover': { bgcolor: palette.grey[300] },
          }}
        >
          <ContentCopy fontSize="small" />
        </IconButton>
      </Tooltip>
    </Box>
  )
}
