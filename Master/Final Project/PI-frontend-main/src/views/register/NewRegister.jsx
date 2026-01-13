import logotipo from '../../icon/logotipo.svg'
import { AuthCard } from '../../components/AuthCard'
import { CustomButton } from '../../components/CustomButton'
import { CustomSeparator } from '../../components/CustomSeparator'
import { CustomTextLink } from '../../components/CustomTextLink'
import { ArrowForwardIos, ArrowBackIos } from '@mui/icons-material'
import { NewRegisterForm } from './NewRegisterForm'
import React from 'react'
import { useTranslation } from 'react-i18next'
import { Box } from '@mui/material'

export const NewRegister = () => {
  const { t } = useTranslation()

  return (
    <AuthCard
      poster="/poster.png"
      logo={logotipo}
      title={t('register.createAccount')}
      width={{ xs: '450px', md: '450px' }}
      hideLogo
    >
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          gap: 4,
        }}
      >
        <NewRegisterForm
          ButtonSubmit={({ ...other }) => (
            <CustomButton fullWidth {...other}>
              {t('register.submit')}
            </CustomButton>
          )}
          ButtonBack={({ ...other }) => (
            <CustomButton
              fullWidth
              styleVariant="inverted"
              startIcon={<ArrowBackIos />}
              {...other}
            >
              {t('register.back')}
            </CustomButton>
          )}
          ButtonNext={({ ...other }) => (
            <CustomButton fullWidth endIcon={<ArrowForwardIos />} {...other}>
              {t('register.next')}
            </CustomButton>
          )}
        />
        <CustomSeparator> {t('register.separator')} </CustomSeparator>
        <CustomTextLink
          sx={{
            width: '100%',
            justifyContent: 'center',
            gap: 0,
          }}
          questionText={t('register.signInQuestion')}
          linkText={t('register.signInButton')}
          to={'/signin'}
        />
      </Box>
    </AuthCard>
  )
}
