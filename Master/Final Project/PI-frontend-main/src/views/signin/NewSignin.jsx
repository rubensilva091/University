import React, { useState } from 'react'
import { useSelector } from 'react-redux'
import { Navigate } from 'react-router-dom'
import logotipo from '../../icon/logotipo.svg'
import { useLocalStorageState } from '../../utils/utils'
import { AuthCard } from '../../components/AuthCard'
import { NewSigninMagicLink } from './NewSigninMagicLink'
import { useTranslation } from 'react-i18next'
import { NewSigninPassword } from './NewSigninPassword'

export const NewSignin = () => {
  const [userToken] = useLocalStorageState('authTokens')
  const globalFailure = useSelector((state) => state.global.failure)
  const [authMethod, setAuthMethod] = useState(0)

  const { t } = useTranslation()

  if (
    userToken &&
    userToken !== 'undefined' &&
    userToken !== 'null' &&
    !globalFailure
  ) {
    return <Navigate to="/"></Navigate>
  }

  return (
    <AuthCard
      poster="/poster.png"
      logo={logotipo}
      title={t('login.login')}
      width={{ xs: '400px', sm: '450px' }}
      hideLogo
    >
      {authMethod === 0 ? (
        <NewSigninMagicLink setAuthMethod={() => setAuthMethod(1)} />
      ) : (
        <NewSigninPassword setAuthMethod={() => setAuthMethod(0)} />
      )}
    </AuthCard>
  )
}
