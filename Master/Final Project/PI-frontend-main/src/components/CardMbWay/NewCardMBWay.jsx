import React from 'react'
import { Box, IconButton, Modal, Typography, useTheme } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { Close } from '@mui/icons-material'
import mbWay from '../../icon/mbWay.svg'

export function NewCardMBWay({ isOpen, close, amount, phone, orderID, time }) {
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
          <img src={mbWay} alt="image-mbway" width={150} />
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
            <Typography>{t('paymentReference.orderID')}:</Typography>
            <Typography sx={{ px: 4 }}>{orderID}</Typography>
            <Typography>{t('paymentReference.amount')}:</Typography>
            <Typography sx={{ px: 4 }}>{amount}€</Typography>
            <Typography>{t('paymentReference.phoneNumber')}:</Typography>
            <Typography sx={{ px: 4 }}>{phone}</Typography>
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
              {t('paymentReference.countdown')}:
            </Typography>
            <Typography sx={{ fontSize: 18, fontWeight: 500 }}>
              {time}
            </Typography>
          </Box>
          <Typography>{t('paymentReference.helperTextMBway')}</Typography>
        </Box>
      </Box>
    </Modal>
  )
}
