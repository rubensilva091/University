import CssBaseline from '@mui/material/CssBaseline'
import AppBar from '@mui/material/AppBar'
import Box from '@mui/material/Box'
import Button from '@mui/material/Button'
import Container from '@mui/material/Container'
import MenuItem from '@mui/material/MenuItem'
import Select from '@mui/material/Select'
import TextField from '@mui/material/TextField'
import Toolbar from '@mui/material/Toolbar'
import Typography from '@mui/material/Typography'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import { Link, Navigate } from 'react-router-dom'
import Toast from '../../components/Toast'
import logotipo from '../../icon/logotipo.svg'
import actions from './redux'
import { useLocalStorageState, validateEmail } from './../../utils/utils'

export const SigninView = () => {
  const [userToken] = useLocalStorageState('authTokens')

  const [email, setEmail] = useState('')
  const [time, setTime] = useState(0)
  const [notYet, setNotYet] = useState(false)
  const [invalidEmail, setInvalidEmail] = useState(false)

  const languageStored = localStorage.getItem('i18nextLng')
  if (!languageStored || languageStored === 'en-US') {
    localStorage.setItem('i18nextLng', 'pt')
  }
  const [failedMessage, setFailedMessage] = useState(null)

  const dispatch = useDispatch()
  const success = useSelector((state) => state.signin.success)
  const failure = useSelector((state) => state.signin.failure)
  const statusCode = useSelector((state) => state.signin.statusCode)

  const globalFailure = useSelector((state) => state.global.failure)

  const {
    t,
    i18n: { changeLanguage, language },
  } = useTranslation()

  const notYetMessage = `Please wait ${time} seconds to resend login link`

  const handleLoginSubmit = (event) => {
    event.preventDefault()
    if (time <= 0 || statusCode === 404) {
      logIn()
    } else {
      notAvailable()
    }
  }

  const logIn = () => {
    if (validateEmail(email)) {
      dispatch(actions.newLoginRequest(email))
      setNotYet(false)
      setTime(30)
    } else {
      setFailedMessage(t('toasts.invalidEmail'))
      setInvalidEmail(true)
    }
  }

  const notAvailable = () => {
    setNotYet(true)
  }

  useEffect(() => {
    changeLanguage('pt')
    setFailedMessage(t('toasts.emailFail'))
    const interval = setInterval(() => {
      setTime((prevTime) => prevTime - 1)
    }, 1000)

    return () => clearInterval(interval)
  }, [])

  useEffect(() => {
    if (statusCode === 404) {
      setFailedMessage(t('toasts.login404'))
    }
  }, [statusCode])

  useEffect(() => {
    if (invalidEmail) setInvalidEmail(false)
  }, [email])

  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,

    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  const simpleLink = {
    color: '#fe902b',
    fontSize: '0.75rem',
  }

  const successMessage = t('toasts.emailOk')

  const handleChangeLanguage = () => {
    const newLanguage = language === 'en' ? 'pt' : 'en'
    changeLanguage(newLanguage)
  }

  if (
    userToken &&
    userToken !== 'undefined' &&
    userToken !== 'null' &&
    !globalFailure
  ) {
    return <Navigate to="/"></Navigate>
  }

  return (
    <>
      <AppBar position="static">
        <Toolbar
          sx={{ justifyContent: 'space-between', backgroundColor: 'white' }}
        >
          <Link to="/">
            <img src={logotipo} alt="image" height="50px" />
          </Link>
          <Select
            size="small"
            labelId="demo-simple-select-label"
            id="demo-simple-select"
            value={language}
            // label="Language"
            onChange={handleChangeLanguage}
          >
            <MenuItem value={'pt'}>{t('language.portuguese')}</MenuItem>
            <MenuItem value={'en'}>{t('language.english')}</MenuItem>
          </Select>
        </Toolbar>
      </AppBar>
      <Container component="form" maxWidth="xs" onSubmit={handleLoginSubmit}>
        <CssBaseline />
        <Box
          sx={{
            marginBottom: 0,
            marginTop: 3,
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'flex-start',
          }}
        >
          <Typography
            component="h1"
            variant="h4"
            marginBottom="20px"
            color="#8984f0"
            fontWeight="fontWeightBold"
          >
            {t('login.login')}
          </Typography>
          <TextField
            required
            fullWidth
            id="email"
            label="Email"
            name="email"
            autoComplete="email"
            autoFocus
            onChange={(e) => {
              setEmail(e.target.value.split(' ').join(''))
            }}
            inputProps={{ style: { textTransform: 'lowercase' } }}
          />
          <Button
            component={Link}
            to={'/register'}
            color="secondary"
            style={simpleLink}
          >
            {t('login.createAccount')}
          </Button>
        </Box>
        <Box
          sx={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'flex-start',
          }}
        >
          {success ? (
            <Toast status={true} message={successMessage}></Toast>
          ) : null}
          {failure || invalidEmail ? (
            <Toast status={false} message={failedMessage}></Toast>
          ) : null}
          <Button type="submit" size="medium" variant="contained" style={style}>
            {time <= 0 || statusCode === 404 ? (
              <>{t('loginMagicLink.buttonSubmit')}</>
            ) : (
              <>{time}</>
            )}
          </Button>
        </Box>
        {notYet ? <Toast status={false} message={notYetMessage}></Toast> : null}
      </Container>
    </>
  )
}
