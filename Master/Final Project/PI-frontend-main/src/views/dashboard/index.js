import React, { useEffect, useState } from 'react'
import { useSelector } from 'react-redux'
import CircularLoading from '../../components/CircularLoading'
import { Roles, capitalizeWords } from '../../utils/utils'
import { DashboardAdminView } from './admin'
import { DashboardAssociateView } from './associate'
import { Navigate } from 'react-router-dom'
import StatusError from '../../components/StatusError'

export const DashboardView = () => {
  const [validUntil, setValidUntil] = useState(undefined)
  const loading = useSelector((state) => state.global.loading)
  const success = useSelector((state) => state.global.success)
  const failure = useSelector((state) => state.global.failure)
  const status = useSelector((state) => state.global.status)

  const firstName = useSelector((state) => state.global.firstName)
  const lastName = useSelector((state) => state.global.lastName)
  const role = useSelector((state) => state.global.role)

  const subscriptionStatus = useSelector(
    (state) => state.global.subscriptionStatus,
  )
  const subscriptionHistory = useSelector(
    (state) => state.global.subscriptionHistory,
  )

  useEffect(() => {
    if (subscriptionHistory && subscriptionHistory.length > 0) {
      setValidUntil(subscriptionHistory[0].end_date.split('T')[0])
    }
  }, [success])

  if (loading) {
    return <CircularLoading />
  }

  if (failure && status) {
    const url = `/blank?status=${status}`
    return <Navigate to={url} />
  }
  if (failure && !status) {
    return <StatusError status={status} />
  }

  if (role === Roles.Associate) {
    return (
      <DashboardAssociateView
        valid={subscriptionStatus === 'Valid'}
        name={capitalizeWords(firstName + ' ' + lastName)}
        validUntil={validUntil}
      />
    )
  }

  if (role === Roles.Admin) {
    return <DashboardAdminView />
  }
}
