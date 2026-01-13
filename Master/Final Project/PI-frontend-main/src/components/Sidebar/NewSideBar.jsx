import React, { useEffect, useState } from 'react'
import { useSelector } from 'react-redux'
import { useLocation } from 'react-router-dom'
import { useTranslation } from 'react-i18next'
import {
  Dashboard,
  Group,
  Settings,
  Logout,
  Payment,
  Summarize,
} from '@mui/icons-material'
import { Box, Divider, Stack, Typography } from '@mui/material'
import { useLocalStorageState, Roles, capitalizeWords } from '../../utils/utils'
import { CustomButton } from '../CustomButton'
import { CustomAvatar } from '../CustomAvatar'

export const NewSideBar = () => {
  const { t } = useTranslation()
  const [, setAuth] = useLocalStorageState('authTokens')
  const [options, setOptions] = useState([])

  const firstName = useSelector((state) => state.global.firstName)
  const lastName = useSelector((state) => state.global.lastName)
  const email = useSelector((state) => state.global.email)
  const role = useSelector((state) => state.global.role)

  const { pathname } = useLocation()

  useEffect(() => {
    if (role === Roles.Admin) {
      setOptions(adminOptions)
    }
    if (role === Roles.Associate) {
      setOptions(associateOptions)
    }
  }, [role])

  const associateOptions = [
    { url: '/', name: 'sideBar.dashboard', icon: <Dashboard /> },
    { url: '/quotas', name: 'sideBar.quotas', icon: <Summarize /> },
    { url: '/payment', name: 'sideBar.payment', icon: <Payment /> },
  ]

  const adminOptions = [
    { url: '/', name: 'Dashboard', icon: <Dashboard /> },
    { url: '/associates', name: 'sideBar.associate', icon: <Group /> },
    {
      url: '/paymentSettings',
      name: 'sideBar.paymentSettings',
      icon: <Settings />,
    },
  ]

  const logOut = () => {
    setAuth(undefined)
  }

  return (
    <>
      <Stack height="100%">
        {options.map((opts) => (
          <SideButton
            key={opts.name}
            {...opts}
            selected={opts.url === pathname}
          />
        ))}
      </Stack>
      <Divider variant="middle" sx={{ borderBottomWidth: '2px', mx: 4 }} />
      <Stack py={6} px={4} spacing={4} alignItems="center">
        <CustomAvatar
          name={capitalizeWords(`${firstName} ${lastName}`)}
          sx={{ width: 48, height: 48 }}
        />
        <Stack alignItems="center">
          {role === 'associate' ? (
            <Typography
              fontWeight={500}
              sx={{
                overflow: 'hidden',
                textOverflow: 'ellipsis',
              }}
            >
              {capitalizeWords(`${firstName} ${lastName}`)}
            </Typography>
          ) : null}
          <Typography
            fontWeight={500}
            sx={{
              overflow: 'hidden',
              textOverflow: 'ellipsis',
            }}
          >
            {email}
          </Typography>
        </Stack>
      </Stack>
      <Divider variant="middle" sx={{ borderBottomWidth: '2px', mx: 4 }} />
      <Box sx={{ display: 'flex', justifyContent: 'center', px: 6, py: 4 }}>
        <CustomButton
          to="/sign-in"
          onClick={logOut}
          color="error"
          endIcon={
            <Logout sx={{ stroke: 'currentColor', strokeWidth: '0.7px' }} />
          }
          sx={{
            width: '100%',
          }}
          typography={{
            sx: {
              fontSize: '1.05rem',
              fontWeight: 600,
              mr: 1,
            },
          }}
        >
          {t('sideBar.logout')}
        </CustomButton>
      </Box>
    </>
  )
}

const SideButton = ({ url, name, icon, selected }) => {
  const { t } = useTranslation()
  return (
    <CustomButton
      styleVariant="toggle"
      to={selected ? undefined : url}
      selected={selected}
      sx={{
        px: 6,
        py: 6,
        borderRadius: 0,
        minWidth: 'fit-content',
        width: '100%',
        display: 'flex',
        gap: 6,
        justifyContent: 'start',
      }}
    >
      {icon}
      <Typography textTransform="none">{t(name)}</Typography>
    </CustomButton>
  )
}
