import React, { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import logotipo from '../../icon/logotipo.svg'
import { useTranslation } from 'react-i18next'
import {
  Typography,
  AppBar,
  Button,
  Toolbar,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
} from '@mui/material'
import Toast from '../../components/Toast'
import actions from './redux'
import { useSelector, useDispatch } from 'react-redux'

export const BlankView = () => {
  const {
    t,
    i18n: { changeLanguage, language },
  } = useTranslation()
  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,
    width: 120,

    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  const handleChangeLanguage = () => {
    const newLanguage = language === 'en' ? 'pt' : 'en'
    changeLanguage(newLanguage)
  }

  const successRegister = t('toasts.registerTrue')
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
  const successEmail = t('blank.successEmail')
  const failedEmail = t('blank.failedEmail')

  useEffect(() => {
    const interval = setInterval(() => {
      setTime((prevTime) => prevTime - 1)
    }, 1000)

    return () => clearInterval(interval)
  }, [])

  const renderText = () => {
    if (status === '400') {
      return <Typography>{t('blank.createAccountError')}</Typography>
    }

    if (status === '401') {
      return <Typography>{t('blank.401')}</Typography>
    }

    if (status === '404') {
      return <Typography>{t('blank.404')}</Typography>
    }

    if (status === '500') {
      return <Typography>{t('blank.500')}</Typography>
    }

    if (status === 'loginfailed') {
      return <Typography>{t('blank.loginfailed')}</Typography>
    }

    if (accountCreated) {
      if (oldAccount) {
        return (
          <Typography>
            {t('blank.oldAccount')} {accountCreated}
          </Typography>
        )
      }
      return (
        <Typography>
          {t('blank.resendEmail')} {accountCreated}
        </Typography>
      )
    } else {
      return <Typography>{t('blank.createAccount')}</Typography>
    }
  }

  const renderButton = () => {
    if (status === '400') {
      return (
        <Button
          component={Link}
          to={'/'}
          type="submit"
          variant="contained"
          style={style}
        >
          {t('blank.reconfirm')}
        </Button>
      )
    }

    if (status === '401') {
      localStorage.removeItem('authTokens')
      return (
        <Button
          component={Link}
          to={'/sign-in'}
          type="submit"
          variant="contained"
          style={style}
        >
          {t('blank.login')}
        </Button>
      )
    }

    if (status === '404' || status === '500') {
      return (
        <Button
          component={Link}
          to={'/'}
          type="submit"
          variant="contained"
          style={style}
        >
          {t('blank.home')}
        </Button>
      )
    }

    if (status === 'loginfailed') {
      ;<Button
        component={Link}
        to={'/signin'}
        type="submit"
        variant="contained"
        style={style}
      >
        {t('blank.signin')}
      </Button>
    }

    if (accountCreated) {
      if (time <= 0) {
        return (
          <Button
            variant="contained"
            onClick={resendEmail}
            size="medium "
            style={style}
          >
            resend
          </Button>
        )
      } else {
        return (
          <Button
            variant="contained"
            onClick={resendEmail}
            size="medium "
            style={style}
            disabled
          >
            {time}
          </Button>
        )
      }
    } else {
      return (
        <Button
          component={Link}
          to={'/sign-in'}
          type="submit"
          variant="contained"
          style={style}
        >
          {t('blank.login')}
        </Button>
      )
    }
  }
  return (
    <>
      <AppBar sx={{ boxShadow: 1 }} position="relative">
        <Toolbar
          variant="regular"
          sx={{
            justifyContent: 'space-between',
            backgroundColor: 'white',
            boxShadow: 0,
          }}
        >
          <Link to={'/'}>
            <img src={logotipo} alt="image" height="50px" />
          </Link>
          <FormControl size="small" sx={{ width: '85px' }}>
            <InputLabel id="demo-simple-select-label">
              {/* {t("language.language")} */}
            </InputLabel>
            <Select
              labelId="demo-simple-select-label"
              id="demo-simple-select"
              value={language}
              // label="Language"
              onChange={handleChangeLanguage}
              sx={{ fontSize: 8 }}
            >
              <MenuItem value={'pt'}>{t('language.portuguese')}</MenuItem>
              <MenuItem value={'en'}>{t('language.english')}</MenuItem>
            </Select>
          </FormControl>
        </Toolbar>
      </AppBar>
      <div
        style={{
          display: 'flex',
          paddingTop: 30,
          paddingLeft: 30,
          flexDirection: 'column',
        }}
      >
        {renderText()}
        <div style={{ display: 'flex', paddingTop: 10, paddingLeft: 0 }}>
          {renderButton()}
        </div>
      </div>
      {accountCreated && !oldAccount ? (
        <>
          <Toast status={true} message={successRegister}></Toast>
        </>
      ) : null}
      {success ? (
        <>
          <Toast status={true} message={successEmail}></Toast>
        </>
      ) : null}
      {failure ? (
        <>
          <Toast status={false} message={failedEmail}></Toast>
        </>
      ) : null}
    </>
  )
}
