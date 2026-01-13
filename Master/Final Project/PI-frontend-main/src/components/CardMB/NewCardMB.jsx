import React from 'react'
import mb from '../../icon/mb.svg'
import { IconButton, Modal, Box, Typography, useTheme } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { Close } from '@mui/icons-material'
import { ClipboardText } from '../ClipboardText/ClipboardText'
import './NewCardMB.css'

export function NewCardMB({
  isOpen,
  close,
  reference,
  entity,
  amount,
  expiricyDate,
}) {
  const { t } = useTranslation()
  const { palette } = useTheme()

  return (
    <Modal
      open={isOpen}
      onClose={close}
      sx={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <Box
        sx={{
          position: 'relative',
          width: '100%',
          maxWidth: 500,
          margin: 2,
          bgcolor: 'background.paper',
          boxShadow: 24,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          gap: 8,
          p: 8,
          borderRadius: 4,
        }}
      >
        <Box
          sx={{
            width: '100%',
            display: 'flex',
            justifyContent: 'center',
          }}
        >
          <IconButton
            onClick={close}
            sx={{
              position: 'absolute',
              right: 10,
              top: 10,
              color: palette.common.darkGray,
            }}
          >
            <Close />
          </IconButton>
          <img src={mb} alt="image" width={150} />
        </Box>
        <Box
          sx={{
            width: '100%',
            display: 'flex',
            flexDirection: 'column',
            gap: 2,
          }}
        >
          <Typography sx={{ fontSize: 20, fontWeight: 600, pb: 2 }}>
            {t('paymentReference.title')}
          </Typography>
          <div className="cardMBGrid">
            <Typography>{t('paymentReference.reference')}:</Typography>
            <ClipboardText text={reference} />
            <Typography>{t('paymentReference.entity')}:</Typography>
            <ClipboardText text={entity} />
            <Typography>{t('paymentReference.amount')}:</Typography>
            <ClipboardText text={amount + '€'} />
          </div>
        </Box>
        <Box
          sx={{
            width: '100%',
            display: 'flex',
            flexDirection: 'column',
            gap: 2,
          }}
        >
          <Box sx={{ display: 'flex', gap: 4, pb: 4 }}>
            <Typography sx={{ fontSize: 18 }}>
              {t('paymentReference.expiricyDate')}
            </Typography>
            <Typography sx={{ fontSize: 18, fontWeight: 500 }}>
              {expiricyDate}
            </Typography>
          </Box>
          <Typography>{t('paymentReference.helperTextMB')}</Typography>
        </Box>
      </Box>
    </Modal>
  )
}
