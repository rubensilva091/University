import React from 'react'
import Box from '@mui/material/Box'
import { useTranslation } from 'react-i18next'
import { NewStatusCard } from '../../../components/StatusCard/NewStatusCard'
import logotipo from '../../../icon/logotipo.svg'
import { NewQRCode } from '../../../components/NewQRCode/NewQRCode'

export const NewAssociateView = ({ valid, name, validUntil, id }) => {
  const { t } = useTranslation()
  return (
    <Box
      sx={{
        width: '100%',
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        gap: 10,
      }}
    >
      <Box
        sx={{
          display: 'flex',
          maxHeight: '110px',
        }}
      >
        <img src={logotipo} alt="image" height="100px" />
      </Box>
      <NewQRCode name={name} id={id} />
      <Box sx={{ width: '45vh' }}>
        <NewStatusCard valid={valid} validUntil={validUntil} />
      </Box>
    </Box>
  )
}
