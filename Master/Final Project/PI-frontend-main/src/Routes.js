import React from 'react'
import { Navigate, RouterProvider, createBrowserRouter } from 'react-router-dom'
import {
  BlankView,
  DashboardView,
  PaymentView,
  QuotasView,
  RegisterView,
  ScanView,
  SigninView,
  AdminView,
  PaymentSettings,
} from './views'
import Layout from './components/Layout'
import { ProtectedRoute } from './utils/ProtectedRoute'
import { Associates } from './views/associates'
import { NewSignin } from './views/signin/NewSignin'
import { NewRegister } from './views/register/NewRegister'
import { NewBlank } from './views/blank/NewBlank'
import { NewMainPage } from './views/dashboard/NewMainPage'
import { NewLayout } from './components/Layout/NewLayout'
import { NewPaymentHistory } from './views/quotas/NewPaymentHistory'
import { NewPaymentLayout } from './views/payment/NewPaymentLayout'

const appVersion = process.env.REACT_APP_VERSION

const router_0 = createBrowserRouter([
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <Layout />
      </ProtectedRoute>
    ),
    children: [
      { element: <DashboardView />, index: true },
      { path: '/associates', element: <Associates /> },
      { path: '/associates/account', element: <AdminView /> },
      { path: '/quotas', element: <QuotasView /> },
      { path: '/payment', element: <PaymentView /> },
      { path: '/paymentSettings', element: <PaymentSettings /> },
    ],
  },
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <Layout />
      </ProtectedRoute>
    ),
    children: [{ path: '/scan', element: <ScanView /> }],
  },
  { path: '/sign-in', element: <SigninView /> },
  { path: '/register', element: <RegisterView /> },
  { path: '/blank', element: <BlankView /> },
  { path: '*', element: <Navigate to="/" replace /> },
])

const router_05 = createBrowserRouter([
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <Layout />
      </ProtectedRoute>
    ),
    children: [
      { element: <DashboardView />, index: true },
      { path: '/associates', element: <Associates /> },
      { path: '/associates/account', element: <AdminView /> },
      { path: '/quotas', element: <QuotasView /> },
      { path: '/payment', element: <PaymentView /> },
      { path: '/paymentSettings', element: <PaymentSettings /> },
    ],
  },
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <Layout />
      </ProtectedRoute>
    ),
    children: [{ path: '/scan', element: <ScanView /> }],
  },
  { path: '/sign-in', element: <NewSignin /> },
  { path: '/register', element: <NewRegister /> },
  { path: '/blank', element: <NewBlank /> },
  { path: '*', element: <Navigate to="/" replace /> },
])

const router_1 = createBrowserRouter([
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <NewLayout />
      </ProtectedRoute>
    ),
    children: [
      { element: <NewMainPage />, index: true },
      { path: '/associates', element: <Associates /> },
      { path: '/associates/account', element: <AdminView /> },
      { path: '/quotas', element: <NewPaymentHistory /> },
      { path: '/payment', element: <NewPaymentLayout /> },
      { path: '/paymentSettings', element: <PaymentSettings /> },
    ],
  },
  {
    path: '/',
    element: (
      <ProtectedRoute>
        <NewLayout />
      </ProtectedRoute>
    ),
    children: [{ path: '/scan', element: <ScanView /> }],
  },
  { path: '/sign-in', element: <NewSignin /> },
  { path: '/register', element: <NewRegister /> },
  { path: '/blank', element: <NewBlank /> },
  { path: '*', element: <Navigate to="/" replace /> },
])

function Routes() {
  const queryParameters = new URLSearchParams(window.location.search)
  const refreshToken = queryParameters.get('refresh')
  const token = queryParameters.get('token')
  if ((!!refreshToken || !!token) && token) {
    localStorage.setItem('authTokens', JSON.stringify('Bearer ' + token))
  }
  switch (appVersion) {
    case '1':
      return <RouterProvider router={router_1} />
    case '0.5':
      return <RouterProvider router={router_05} />
    default:
      return <RouterProvider router={router_0} />
  }
}

export default Routes
