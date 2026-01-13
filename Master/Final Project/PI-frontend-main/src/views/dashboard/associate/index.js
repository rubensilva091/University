import React from 'react'
import Box from '@mui/material/Box'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import Card from '@mui/material/Card'
import StatusCard from '../../../components/StatusCard'

export const DashboardAssociateView = ({ valid, name, validUntil }) => {
  const { t } = useTranslation()
  return (
    <>
      <Box>
        <Typography
          color="#fe902b"
          fontWeight="fontWeightBold"
          fontSize="1.5rem"
          mb="20px"
        >
          {t('dashboard.hello')}, {name}!
        </Typography>
      </Box>
      <Box sx={{ width: '45vh' }}>
        <Card
          variant="outlined"
          sx={{
            boxShadow: 5,
            color: 'rgba(137, 132, 240)',
            background: 'rgba(137, 132, 240,0.1)',
            border: 0,
          }}
        >
          <StatusCard valid={valid} validUntil={validUntil} />
        </Card>
      </Box>
    </>
  )
}
