import CancelIcon from '@mui/icons-material/Cancel'
import { Typography } from '@mui/material'
import Box from '@mui/material/Box'
import Card from '@mui/material/Card'
import CardContent from '@mui/material/CardContent'
import React from 'react'
import { useTranslation } from 'react-i18next'

const StatusError = ({ status }) => {
  const { t } = useTranslation()
  return (
    <Box sx={{ width: '45vh' }}>
      <Card
        variant="outlined"
        sx={{
          boxShadow: 5,
          color: 'rgba(137, 132, 240)',
          background: 'rgba(137, 132, 240,0.3)',
          border: 3,
        }}
      >
        <React.Fragment>
          <CardContent>
            <Box sx={{ display: 'flex', alignItems: 'center' }}>
              <CancelIcon sx={{ color: 'red', fontSize: 40 }}></CancelIcon>
              <Typography color="black">
                {t('error.start')} {status} {t('error.end')}
              </Typography>
            </Box>
          </CardContent>
        </React.Fragment>
      </Card>
    </Box>
  )
}

export default StatusError
