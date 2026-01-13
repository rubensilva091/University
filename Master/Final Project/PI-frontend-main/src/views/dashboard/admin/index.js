import Box from '@mui/material/Box'
import React, { useEffect, useState } from 'react'
import { useDispatch, useSelector } from 'react-redux'
import { Navigate } from 'react-router-dom'
import CircularLoading from '../../../components/CircularLoading'
import StatusError from '../../../components/StatusError'
import actions from './redux'
import PieChart from '../../../components/PieChart'
import LineChart from '../../../components/LineChart'
import Grid from '@mui/material/Grid'
import { Typography } from '@mui/material'
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
  ArcElement,
  Colors,
} from 'chart.js'
import { useTranslation } from 'react-i18next'

ChartJS.register(
  ArcElement,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
  Colors,
)

export const DashboardAdminView = () => {
  const [, setAuthTokens] = useState([])
  const loading = useSelector((state) => state.adminDashboard.loading)
  const failure = useSelector((state) => state.adminDashboard.failure)
  const statusCode = useSelector((state) => state.adminDashboard.status)
  const dispatch = useDispatch()
  const associatesHistory = useSelector(
    (state) => state.adminDashboard.associatesHistory,
  )
  const activeAssociates = useSelector(
    (state) => state.adminDashboard.activeAssociates,
  )
  const inactiveAssociates = useSelector(
    (state) => state.adminDashboard.inactiveAssociates,
  )
  const monthlyIncome = useSelector(
    (state) => state.adminDashboard.monthlyIncome,
  )
  const havePaid = useSelector((state) => state.adminDashboard.havePaid)
  const neverPaid = useSelector((state) => state.adminDashboard.neverPaid)
  const totalAssociates = useSelector(
    (state) => state.adminDashboard.totalAssociates,
  )
  const { t } = useTranslation()

  const SubsData = [
    {
      name: t('adminDashboard.validSubs'),
      users: activeAssociates,
    },
    {
      name: t('adminDashboard.expiredSubs'),
      users: inactiveAssociates,
    },
  ]
  const historicPayersData = [
    {
      name: t('adminDashboard.amountPayers'),
      users: havePaid,
    },
    {
      name: t('adminDashboard.amountNotPayers'),
      users: neverPaid,
    },
  ]

  const currentSubsChartData = {
    labels: SubsData?.map((data) => data.name),
    datasets: [
      {
        label: 'Nº Associates',
        data: SubsData?.map((data) => data.users),
        backgroundColor: ['#8984f0', 'rgba(153, 102, 255, 0.2)'],
        borderColor: 'black',
        borderWidth: 2,
      },
    ],
  }
  const historicPayersChartData = {
    labels: historicPayersData?.map((data) => data.name),
    datasets: [
      {
        label: 'Nº Associates',
        data: historicPayersData?.map((data) => data.users),
        backgroundColor: ['#8984f0', 'rgba(153, 102, 255, 0.2)'],
        borderColor: 'black',
        borderWidth: 2,
      },
    ],
  }
  const associatesChartData = {
    labels: associatesHistory?.map(
      (data) =>
        data.month.Time.split('T')[0].split('-')[1] +
        '/' +
        data.month.Time.split('T')[0].split('-')[0],
    ),
    datasets: [
      {
        label: 'Nº Associates',
        data: associatesHistory?.map((data) => data.number_associates),
        backgroundColor: [
          'rgba(153, 102, 255, 0.2)',
          'rgba(255, 99, 132, 0.2)',
        ],
        borderColor: 'black',
        borderWidth: 2,
      },
    ],
  }
  const monthlyIncomeData = {
    labels: monthlyIncome?.map(
      (data) =>
        data.month.Time.split('T')[0].split('-')[1] +
        '/' +
        data.month.Time.split('T')[0].split('-')[0],
    ),
    datasets: [
      {
        label: '€',
        data: monthlyIncome?.map((data) => data.income),
        backgroundColor: [
          'rgba(153, 102, 255, 0.2)',
          'rgba(255, 99, 132, 0.2)',
        ],
        borderColor: 'black',
        borderWidth: 2,
      },
    ],
  }

  useEffect(() => {
    try {
      const tokens = JSON.parse(localStorage.getItem('authTokens'))
      setAuthTokens(tokens)
      dispatch(actions.fetchSummaryRequest(tokens))
    } catch (err) {
      const queryParameters = new URLSearchParams(window.location.search)
      const refreshToken = queryParameters.get('refresh')
      const token = queryParameters.get('token')
      dispatch(actions.fetchSummaryRequest(refreshToken + ' ' + token))
    }
  }, [])

  if (loading) {
    return <CircularLoading />
  }

  if (failure) {
    if (statusCode) {
      const url = `/blank?status=${statusCode}`
      return <Navigate to={url} />
    }
    return <StatusError statusCode={statusCode} />
  }

  return (
    <Box>
      <Typography
        color="#8984f0"
        fontWeight="fontWeightBold"
        fontSize="1.5rem"
        mb="5px"
      >
        Dashboard
      </Typography>
      <Typography
        color="#8984f0"
        fontWeight="fontWeightBold"
        fontSize="1rem"
        mb="50px"
      >
        {t('adminDashboard.totalVerifiedAssociates')}: {totalAssociates}
      </Typography>
      <Grid
        container
        display="flex"
        spacing={2}
        direction="row"
        justifyContent="center"
        alignItems="center"
      >
        <Grid item xs={12} sm={6} md={4} lg={3}>
          <PieChart
            chartData={historicPayersChartData}
            titleText={t('adminDashboard.historicPayers')}
          />
        </Grid>
        <Grid item xs={12} sm={6} md={4} lg={3}>
          <LineChart
            chartData={associatesChartData}
            titleText={t('adminDashboard.usersGraphTitle')}
          />
        </Grid>
        <Grid item xs={12} sm={6} md={4} lg={3}>
          <PieChart
            chartData={currentSubsChartData}
            titleText={t('adminDashboard.currentPayers')}
          />
        </Grid>
        <Grid item xs={12} sm={6} md={4} lg={3}>
          <LineChart
            chartData={monthlyIncomeData}
            titleText={t('adminDashboard.incomeGraphTitle')}
          />
        </Grid>
      </Grid>
    </Box>
  )
}
