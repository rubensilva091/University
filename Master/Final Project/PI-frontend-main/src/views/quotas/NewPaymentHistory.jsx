import React, { useEffect } from 'react'
import { Navigate } from 'react-router-dom'
import { useTranslation } from 'react-i18next'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import { Box, Typography } from '@mui/material'
import { KeyboardDoubleArrowDownRounded } from '@mui/icons-material'
import { NewPaymentHistorySkeleton } from './NewPaymentHistorySkeleton'
import { NewPaymentHistoryTable } from '../../components/TableQuotas/NewPaymentHistoryTable'
import { CustomButton } from '../../components/CustomButton/index'
import { NewStatusError } from '../../components/StatusError/NewStatusError'

export const NewPaymentHistory = () => {
  const { t } = useTranslation()

  const dispatch = useDispatch()
  const loading = useSelector((state) => state.quotas.loading)
  const failure = useSelector((state) => state.quotas.failure)
  const jwt = useSelector((state) => state.quotas.jwt)
  const nextPage = useSelector((state) => state.quotas.nextPage)
  const pageSize = useSelector((state) => state.quotas.pageSize)
  const subscriptionHistory = useSelector(
    (state) => state.quotas.subscriptionHistory,
  )
  const endOfList = useSelector((state) => state.quotas.endOfList)
  const error = useSelector((state) => state.quotas.error)

  useEffect(() => {
    if (subscriptionHistory.length === 0) {
      const authTokens = JSON.parse(localStorage.getItem('authTokens'))
      dispatch(
        actions.setField({
          jwt: authTokens,
        }),
      )
      dispatch(
        actions.fetchQuotasRequest({
          jwt: authTokens,
          page: 1,
          pageSize: 8,
        }),
      )
    }
  }, [])

  const fetchNextPage = () => {
    dispatch(
      actions.fetchQuotasRequest({
        jwt: jwt,
        page: nextPage,
        pageSize: pageSize,
      }),
    )
  }

  if (loading && subscriptionHistory.length === 0) {
    return <NewPaymentHistorySkeleton />
  }

  if (failure) {
    if (error === 401) {
      return <Navigate to="/blank?status=401" />
    } else if (error) {
      return <Navigate to={`/blank?status=${error}`} />
    }
    return <NewStatusError status={error} />
  }

  return (
    <>
      <Typography color="primary" fontWeight={700} variant="h4">
        {t('table.title')}
      </Typography>

      {subscriptionHistory && subscriptionHistory.length ? (
        <NewPaymentHistoryTable
          data={subscriptionHistory}
          pagination={
            !endOfList ? (
              <Box
                sx={{
                  display: 'flex',
                  flexDirection: 'row',
                  justifyContent: 'center',
                  alignItems: 'center',
                  height: '100%',
                }}
              >
                <CustomButton
                  styleVariant="toggle"
                  sx={{ px: 4, display: 'flex', gap: 1 }}
                  onClick={fetchNextPage}
                  loading={loading}
                  noTypography={true}
                >
                  <KeyboardDoubleArrowDownRounded />
                  <Typography textTransform="none">
                    {t('table.quotes.showMore')}
                  </Typography>
                </CustomButton>
              </Box>
            ) : undefined
          }
        />
      ) : (
        <Typography>{t('quotas.msgStatus')}</Typography>
      )}
    </>
  )
}
