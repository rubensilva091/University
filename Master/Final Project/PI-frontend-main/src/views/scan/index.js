import React, { useEffect } from 'react'
import Box from '@mui/material/Box'
import { Typography } from '@mui/material'
import actions from './redux'
import { useSelector, useDispatch } from 'react-redux'
import StatusCard from '../../components/StatusCard'
import CircularLoading from '../../components/CircularLoading'
import { useNavigate, Navigate } from 'react-router-dom'
import { useLocalStorageState } from '../../utils/utils'
import { useTranslation } from 'react-i18next'

export const ScanView = () => {
  const dispatch = useDispatch()
  const navigate = useNavigate()

  const { t } = useTranslation()
  const [userToken, setUserToken] = useLocalStorageState('authTokens')

  const loading = useSelector((state) => state.scan.loading)
  const failure = useSelector((state) => state.scan.failure)

  const firstName = useSelector((state) => state.scan.firstName)
  const lastName = useSelector((state) => state.scan.lastName)
  const email = useSelector((state) => state.scan.email)
  const nif = useSelector((state) => state.scan.nif)
  const role = useSelector((state) => state.scan.role)
  const subscriptionStatus = useSelector(
    (state) => state.scan.subscriptionStatus,
  )

  const errorStatus = useSelector((state) => state.scan.errorStatus)

  useEffect(() => {
    const queryParameters = new URLSearchParams(window?.location?.search)
    const id = queryParameters.get('id')
    if (!userToken || userToken === 'undefined' || userToken === 'null') {
      return navigate('/sign-in')
    }
    const tokens = JSON.parse(localStorage.getItem('authTokens'))
    dispatch(actions.getAccountRequest({ jwt: tokens, id }))
  }, [])

  if (loading) {
    return <CircularLoading />
  }

  if (failure && errorStatus === 401) {
    return <Navigate to="/blank?status=401" />
  }

  if (failure && errorStatus === 404) {
    return <Navigate to="/blank?status=404" />
  }

  if (failure) {
    return <Navigate to="/blank?status=500" />
  }

  return (
    <Box>
      <Typography>
        {t('sideBar.name')}: {firstName} {lastName}
      </Typography>
      <Typography>
        {t('sideBar.email')}: {email}
      </Typography>
      <Typography>NIF: {nif}</Typography>
      <Typography style={{ paddingBottom: 20 }}>
        {t('register.category')}: {role}
      </Typography>
      <StatusCard valid={subscriptionStatus === 'Valid'} />
    </Box>
  )
}
