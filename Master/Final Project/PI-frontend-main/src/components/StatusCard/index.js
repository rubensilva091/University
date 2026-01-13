import React from 'react'
import { Link } from 'react-router-dom'
import Box from '@mui/material/Box'
import { Typography } from '@mui/material'
import { useTranslation } from 'react-i18next'
import CardActions from '@mui/material/CardActions'
import CardContent from '@mui/material/CardContent'
import Button from '@mui/material/Button'
import CheckIcon from '@mui/icons-material/Check'
import CancelIcon from '@mui/icons-material/Cancel'
import Card from '@mui/material/Card'

const StatusCard = ({ valid, validUntil }) => {
  const { t } = useTranslation()

  const ValidCard = () => {
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
                <CheckIcon
                  sx={{ color: 'green', fontSize: 40, pr: '16px' }}
                ></CheckIcon>
                <div style={{ flexDirection: 'column' }}>
                  <Typography color="black">
                    {t('dashboard.trueSentence')}
                  </Typography>
                  {validUntil ? (
                    <Typography color="black">
                      {t('dashboard.validUntil')} {validUntil}
                    </Typography>
                  ) : null}
                </div>
              </Box>
            </CardContent>
            <CardActions
              sx={{
                padding: '0px',
                display: 'flex',
                justifyContent: 'end',
                pr: '16px',
              }}
            >
              <Button size="small" component={Link} to={'/quotas'}>
                {t('dashboard.learnMore')}
              </Button>
            </CardActions>
          </React.Fragment>
        </Card>
      </Box>
    )
  }

  const InvalidCard = () => {
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
                  {t('dashboard.falseSentence')}
                </Typography>
              </Box>
            </CardContent>
            <CardActions sx={{ padding: '0px', ml: '16px' }}>
              <Button size="small" component={Link} to={'/quotas'}>
                {t('dashboard.learnMore')}
              </Button>
            </CardActions>
          </React.Fragment>
        </Card>
      </Box>
    )
  }

  if (valid) {
    return <ValidCard />
  } else {
    return <InvalidCard />
  }
}
export default StatusCard
