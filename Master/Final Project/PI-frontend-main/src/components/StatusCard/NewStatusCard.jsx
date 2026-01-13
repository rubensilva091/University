import React from 'react'
import { Typography, useTheme, Card, Box, CardActionArea } from '@mui/material'
import { CheckRounded, CloseRounded, CancelRounded } from '@mui/icons-material'
import { useTranslation } from 'react-i18next'
import { CustomButton } from '../CustomButton/index'

export const NewStatusCard = ({ valid, validUntil, status }) => {
  const { t } = useTranslation()
  const { palette } = useTheme()

  const ValidCard = () => {
    return (
      <>
        <CheckRounded
          sx={{
            color: 'green',
            fontSize: 60,
            stroke: 'green',
            strokeWidth: '0.5px',
          }}
        />
        <Box sx={{ width: '100%', display: 'flex', flexDirection: 'column' }}>
          <Typography>{t('dashboard.trueSentence')}</Typography>
          <Typography>
            {`${t('dashboard.validUntil')} ${validUntil}`}
          </Typography>
        </Box>
      </>
    )
  }

  const InvalidCard = () => {
    return (
      <>
        <CloseRounded
          sx={{
            color: 'red',
            fontSize: 60,
            stroke: 'red',
            strokeWidth: '0.5px',
          }}
        />
        <Box sx={{ width: '100%', display: 'flex', flexDirection: 'column' }}>
          <Typography>{t('dashboard.falseSentence')}</Typography>
          <Box sx={{ display: 'flex', justifyContent: 'end' }}>
            <CustomButton styleVariant="link" to={'/quotas'}>
              {t('dashboard.learnMore')}
            </CustomButton>
          </Box>
        </Box>
      </>
    )
  }

  const ErrorCard = () => {
    return (
      <>
        <CancelRounded sx={{ color: 'red', fontSize: 60 }} />
        <Box sx={{ width: '100%', display: 'flex', flexDirection: 'column' }}>
          <Typography color="black">
            {t('error.start')} {status} {t('error.end')}
          </Typography>
        </Box>
      </>
    )
  }

  let displayCard = <></>

  switch (valid) {
    case true:
      displayCard = <ValidCard />
      break
    case false:
      displayCard = <InvalidCard />
      break
    default:
      displayCard = <ErrorCard />
  }

  return (
    <Card
      sx={{
        border: 3,
        borderColor: palette.primary[400],
        borderRadius: 3,
        background: palette.primary[100],
      }}
      raised
    >
      <CardActionArea
        sx={{
          px: 6,
          py: 3,
          display: 'flex',
          alignItems: 'center',
          gap: 4,
        }}
      >
        {displayCard}
      </CardActionArea>
    </Card>
  )
}
