import React, { useEffect, useState } from 'react'
import { useSelector } from 'react-redux'
import CircularLoading from '../../components/CircularLoading'
import { Roles, capitalizeWords } from '../../utils/utils'
import { DashboardAdminView } from './admin'
import { Navigate } from 'react-router-dom'
import { NewAssociateView } from './associate/NewAssociateView'
import { NewStatusError } from '../../components/StatusError/NewStatusError'

export const NewMainPage = () => {
  const [validUntil, setValidUntil] = useState(undefined)
  const loading = useSelector((state) => state.global.loading)
  const success = useSelector((state) => state.global.success)
  const failure = useSelector((state) => state.global.failure)
  const status = useSelector((state) => state.global.status)

  const firstName = useSelector((state) => state.global.firstName)
  const lastName = useSelector((state) => state.global.lastName)
  const id = useSelector((state) => state.global.id)
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
    return <NewStatusError status={status} />
  }

  if (role === Roles.Associate) {
    return (
      <NewAssociateView
        valid={subscriptionStatus === 'Valid'}
        id={id}
        name={capitalizeWords(firstName + ' ' + lastName)}
        validUntil={validUntil}
      />
    )
  }

  if (role === Roles.Admin) {
    return <DashboardAdminView />
  }
}
