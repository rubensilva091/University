import React from 'react'
import { Navigate } from 'react-router-dom'
import { useLocalStorageState } from './utils'
import { useSelector } from 'react-redux'

export const ProtectedRoute = ({ children }) => {
  const [userToken] = useLocalStorageState('authTokens')
  const failure = useSelector((state) => state.global.failure)
  const status = useSelector((state) => state.global.status)

  if (!userToken || userToken === 'undefined' || userToken === 'null') {
    return <Navigate to="/sign-in" />
  }
  console.log(status, failure)

  if (failure) {
    if (status === 401) {
      return <Navigate to="/sign-in" />
    } else {
      return <Navigate to={`/blank?status=500`} />
    }
  }

  return children
}
