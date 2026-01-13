import { useNavigate } from 'react-router-dom'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import actions from './redux'
import { Box, IconButton, InputAdornment, Typography } from '@mui/material'
import { Visibility, VisibilityOff } from '@mui/icons-material'
import Toast from '../../components/Toast'
import { useLocalStorageState, validateEmail } from '../../utils/utils'
import { CustomTextField } from '../../components/CustomInput/CustomTextField'
import { CustomButton } from '../../components/CustomButton'
import { CustomSeparator } from '../../components/CustomSeparator'
import { CustomTextLink } from '../../components/CustomTextLink/index'

export const NewSigninPassword = ({ setAuthMethod }) => {
  const [showPassword, setShowPassword] = useState(false)
  const [, setAuthTokens] = useLocalStorageState('authTokens')
  const navigate = useNavigate()

  const dispatch = useDispatch()
  const loading = useSelector((state) => state.signin.loading)
  const response = useSelector((state) => state.signin.response)
  const email = useSelector((state) => state.signin.email)
  const error = useSelector((state) => state.signin.error)
  const password = useSelector((state) => state.signin.password)

  const { t } = useTranslation()

  const handleLoginSubmit = (event) => {
    event.preventDefault()
    if (error.email === '' && error.credentials === '') {
      if (!validateEmail(email)) {
        dispatch(actions.updateLoginError({ email: 'toasts.invalidEmail' }))
      } else if (password.length < 8) {
        dispatch(actions.updateLoginError({ credentials: 'toasts.login403' }))
      } else {
        dispatch(actions.newLoginPasswordRequest({ email, password }))
      }
    }
  }

  useEffect(() => {
    console.log(response)
    if (response !== undefined) {
      const token = response.data.token
      setAuthTokens('Bearer ' + token)
      dispatch(actions.resetLogin())
      navigate('/dashboard')
    }
  }, [response])

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
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          gap: 4,
        }}
      >
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
          error={error.email !== '' || error.credentials !== ''}
          helperText={error.email ? t(error.email) : undefined}
        />
        <CustomTextField
          required
          type={showPassword ? 'text' : 'password'}
          label={t('loginPassword.password')}
          autoComplete="current-password"
          value={password}
          onValueChange={(value) => {
            dispatch(actions.updateLoginPassword(value))
          }}
          error={error.credentials !== ''}
          InputProps={{
            endAdornment: (
              <InputAdornment position="end">
                <IconButton
                  aria-label="toggle password visibility"
                  edge="end"
                  tabIndex={-1}
                  onClick={() => setShowPassword(!showPassword)}
                >
                  {showPassword ? <VisibilityOff /> : <Visibility />}
                </IconButton>
              </InputAdornment>
            ),
          }}
        />
        <Typography
          variant="body2"
          color="error"
          sx={{ px: 3, display: error.credentials !== '' ? 'block' : 'none' }}
        >
          {t(error.credentials)}
        </Typography>
      </Box>

      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          gap: 0,
        }}
      >
        <CustomTextLink
          questionText={t('loginPassword.passwordRecoveryQuestion')}
          linkText={t('loginPassword.passwordRecoveryButton')}
        />
        <CustomTextLink
          questionText={t('login.createAccountQuestion')}
          linkText={t('login.createAccountButton')}
          to={'/register'}
        />
      </Box>
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
        }}
      >
        <CustomButton type="submit" loading={loading}>
          {t('loginPassword.buttonSubmit')}
        </CustomButton>
        <CustomSeparator>{t('login.separator')}</CustomSeparator>
        <CustomButton styleVariant="inverted" onClick={setAuthMethod}>
          {t('loginPassword.buttonMagicLink')}
        </CustomButton>
      </Box>
      {error.global ? (
        <Toast status={false} message={t(error.global)}></Toast>
      ) : null}
    </Box>
  )
}
