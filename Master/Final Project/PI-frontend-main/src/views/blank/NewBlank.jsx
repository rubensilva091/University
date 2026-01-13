import React, { useState, useEffect } from 'react'
import { SecurityIcon } from '../../icon/SecurityIcon'
import { useTranslation } from 'react-i18next'
import { Box, Typography } from '@mui/material'
import Toast from '../../components/Toast'
import actions from './redux'
import { useSelector, useDispatch } from 'react-redux'
import { AuthCard } from '../../components/AuthCard/index'
import { CustomButton } from '../../components/CustomButton/index'
import { useTheme } from '@emotion/react'
import { CheckIcon } from '../../icon/CheckIcon'

export const NewBlank = () => {
  const { t } = useTranslation()
  const { palette } = useTheme()

  const queryParameters = new URLSearchParams(window.location.search)
  const status = queryParameters.get('status')
  const accountCreated = queryParameters.get('account')
  const oldAccount = queryParameters.get('old')
  const dispatch = useDispatch()
  const failure = useSelector((state) => state.blank.failure)
  const success = useSelector((state) => state.blank.success)
  const [time, setTime] = useState(0)

  const resendEmail = () => {
    dispatch(actions.resendConfirmationRequest(accountCreated))
    setTime(30)
  }

  useEffect(() => {
    const interval = setInterval(() => {
      setTime((prevTime) => prevTime - 1)
    }, 1000)

    return () => clearInterval(interval)
  }, [])

  const renderIcon = () => {
    const typographyStyle = {
      variant: 'h3',
      fontSize: 100,
      fontWeight: 500,
      sx: {
        color: palette.grey[500],
      },
    }
    switch (status) {
      case '400':
        return <Typography {...typographyStyle}>400</Typography>
      case '401':
        return <Typography {...typographyStyle}>401</Typography>
      case '404':
        return <Typography {...typographyStyle}>404</Typography>
      case '500':
        return <Typography {...typographyStyle}>500</Typography>
      default:
        if (!accountCreated) {
          return <CheckIcon height="100%" />
        }
        return <SecurityIcon height="100%" />
    }
  }

  const renderText = () => {
    switch (status) {
      case '400':
        return t('blank.createAccountError')
      case '401':
        return t('blank.401')
      case '404':
        return t('blank.404')
      case '500':
        return t('blank.500')
      case 'loginfailed':
        return t('blank.loginfailed')
      default:
        if (accountCreated) {
          if (oldAccount) {
            return t('blank.oldAccount') + accountCreated
          }
          return t('blank.resendEmail') + accountCreated
        } else {
          return t('blank.createAccount')
        }
    }
  }

  const renderButton = () => {
    if (status === '400') {
      return <CustomButton to={'/'}>{t('blank.reconfirm')}</CustomButton>
    }

    if (status === '401') {
      localStorage.removeItem('authTokens')
      return <CustomButton to={'/sign-in'}>{t('blank.login')}</CustomButton>
    }

    if (status === '404' || status === '500') {
      return <CustomButton to={'/'}>{t('blank.home')}</CustomButton>
    }

    if (status === 'loginfailed') {
      return <CustomButton to={'/signin'}>{t('blank.signin')}</CustomButton>
    }

    if (accountCreated) {
      if (time <= 0) {
        return <CustomButton onClick={resendEmail}>resend</CustomButton>
      } else {
        return <CustomButton disabled>{time}</CustomButton>
      }
    } else {
      return <CustomButton to={'/sign-in'}>{t('blank.login')}</CustomButton>
    }
  }

  return (
    <AuthCard height="fit-content" width="600px">
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          gap: '20px',
        }}
      >
        <Box
          sx={{
            maxHeight: '160px',
          }}
        >
          {renderIcon()}
        </Box>
        <Typography>{renderText()}</Typography>
      </Box>
      {renderButton()}
      {accountCreated && !oldAccount ? (
        <Toast status={true} message={t('toasts.registerTrue')} />
      ) : null}
      {success ? (
        <Toast status={true} message={t('blank.successEmail')} />
      ) : null}
      {failure ? (
        <Toast status={false} message={t('blank.failedEmail')} />
      ) : null}
    </AuthCard>
  )
}
