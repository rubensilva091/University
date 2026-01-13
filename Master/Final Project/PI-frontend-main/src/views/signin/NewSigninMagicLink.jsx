import { Box, Typography } from '@mui/material'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import Toast from '../../components/Toast'
import actions from './redux'
import { validateEmail } from '../../utils/utils'
import { CustomTextField } from '../../components/CustomInput/CustomTextField'
import { CustomButton } from '../../components/CustomButton'
import { CustomSeparator } from '../../components/CustomSeparator'
import { CustomTextLink } from '../../components/CustomTextLink'

const WAIT_TIME_FOR_RESEND = 30

export const NewSigninMagicLink = ({ setAuthMethod }) => {
  const [time, setTime] = useState(0)

  const dispatch = useDispatch()
  const success = useSelector((state) => state.signin.success)
  const failure = useSelector((state) => state.signin.failure)
  const statusCode = useSelector((state) => state.signin.statusCode)
  const email = useSelector((state) => state.signin.email)
  const error = useSelector((state) => state.signin.error)

  const { t } = useTranslation()

  const handleLoginSubmit = (event) => {
    event.preventDefault()

    if (time <= 0 || statusCode === 404) {
      if (validateEmail(email)) {
        dispatch(actions.newLoginMagicLinkRequest(email))
        setTime(WAIT_TIME_FOR_RESEND)
      } else {
        dispatch(actions.updateLoginError({ email: 'toasts.invalidEmail' }))
      }
    }
  }

  useEffect(() => {
    const interval = setInterval(() => {
      setTime((prevTime) => prevTime - 1)
    }, 1000)

    return () => clearInterval(interval)
  }, [])

  useEffect(() => {
    if (failure) setTime(0)
  }, [failure])

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        gap: 6,
      }}
      component="form"
      onSubmit={handleLoginSubmit}
    >
      <Typography variant="body1" sx={{ display: 'flex', gap: 2 }}>
        {t('loginMagicLink.helperText')}
      </Typography>
      <CustomTextField
        required
        label={t('login.email')}
        autoComplete="email"
        autoFocus
        value={email}
        onValueChange={(value) => {
          dispatch(
            actions.updateLoginEmail(
              value.toLocaleLowerCase().split(' ').join(''),
            ),
          )
        }}
        error={error.email !== ''}
        helperText={error.email ? t(error.email) : undefined}
      />
      <CustomTextLink
        questionText={t('login.createAccountQuestion')}
        linkText={t('login.createAccountButton')}
        to={'/register'}
      />
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
        }}
      >
        <CustomButton type="submit">
          {time <= 0
            ? t('loginMagicLink.buttonSubmit')
            : `${t('loginMagicLink.buttonClock')} ${time}s`}
        </CustomButton>
        <CustomSeparator>{t('login.separator')}</CustomSeparator>
        <CustomButton styleVariant="inverted" onClick={setAuthMethod}>
          {t('loginMagicLink.buttonPassword')}
        </CustomButton>
      </Box>
      {success ? (
        <Toast status={true} message={t('toasts.emailOk')}></Toast>
      ) : null}
      {error.global ? (
        <Toast status={false} message={t(error.global)}></Toast>
      ) : null}
    </Box>
  )
}
