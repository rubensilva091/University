import React, { useEffect, useState } from 'react'
import { useDispatch } from 'react-redux'
import Routes from './Routes.js'
import actions from './redux/globalStateRedux.js'
import { useLocalStorageState } from './utils/utils'
import CssBaseline from '@mui/material/CssBaseline'
import { createTheme, ThemeProvider } from '@mui/material/styles'
import { ThemeOptions } from './utils/theme'

const App = () => {
  const dispatch = useDispatch()
  const [authTokens, setAuthTokens] = useLocalStorageState('authTokens')

  useEffect(() => {
    const queryParameters = new URLSearchParams(window.location.search)
    const refreshToken = queryParameters.get('refresh')
    const token = queryParameters.get('token')
    if (!!refreshToken || !!token) {
      setAuthTokens('Bearer ' + token)
    }
  }, [])

  useEffect(() => {
    if (authTokens && authTokens !== 'undefined' && authTokens !== 'null') {
      try {
        dispatch(actions.fetchAccountRequest(authTokens))
      } catch (err) {
        console.error(err)
      }
    }
  }, [authTokens])

  const [mode, setMode] = useState('light')
  const defaultTheme = createTheme(ThemeOptions(mode))

  return (
    <ThemeProvider theme={defaultTheme}>
      <CssBaseline />
      <Routes />
    </ThemeProvider>
  )
}

export default App
