import DashboardIcon from '@mui/icons-material/Dashboard'
import GroupIcon from '@mui/icons-material/Group'
import Settings from '@mui/icons-material/Settings'
import LogoutIcon from '@mui/icons-material/Logout'
import PaymentIcon from '@mui/icons-material/Payment'
import SummarizeIcon from '@mui/icons-material/Summarize'
import Avatar from '@mui/material/Avatar'
import Box from '@mui/material/Box'
import Divider from '@mui/material/Divider'
import IconButton from '@mui/material/IconButton'
import List from '@mui/material/List'
import ListItemButton from '@mui/material/ListItemButton'
import ListItemIcon from '@mui/material/ListItemIcon'
import ListItemText from '@mui/material/ListItemText'
import Stack from '@mui/material/Stack'
import Toolbar from '@mui/material/Toolbar'
import Typography from '@mui/material/Typography'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useSelector } from 'react-redux'
import { Link, useLocation } from 'react-router-dom'
import { capitalizeWords, useLocalStorageState, Roles } from '../../utils/utils'

function Side() {
  const { t } = useTranslation()
  const [, setAuth] = useLocalStorageState('authTokens')
  const [options, setOptions] = useState([])

  const firstName = useSelector((state) => state.global.firstName)
  const lastName = useSelector((state) => state.global.lastName)
  const email = useSelector((state) => state.global.email)
  const nif = useSelector((state) => state.global.nif)
  const id = useSelector((state) => state.global.id)
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
    { url: '/', name: 'sideBar.dashboard', icon: <DashboardIcon /> },
    { url: '/quotas', name: 'sideBar.quotas', icon: <SummarizeIcon /> },
    { url: '/payment', name: 'sideBar.payment', icon: <PaymentIcon /> },
  ]

  const adminOptions = [
    { url: '/', name: 'Dashboard', icon: <DashboardIcon /> },
    { url: '/associates', name: 'sideBar.associate', icon: <GroupIcon /> },
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
    <Box flexGrow="1" display="flex" flexDirection="column" width={260}>
      <Toolbar>
        {/* <img src={logotipo} alt="image" max-width="100%" height="35px" /> */}
      </Toolbar>
      <Divider />
      <Box flexGrow="1">
        <List>
          {options.map((opts) => (
            <SideButton
              key={opts.name}
              {...opts}
              selected={opts.url === pathname}
            />
          ))}
        </List>
      </Box>
      <Divider />
      <Stack py={4} alignContent="start">
        <Box
          sx={{
            display: 'flex',
            justifyContent: 'center',
            pb: '10px',
          }}
        >
          <Avatar alt="Remy Sharp" />
        </Box>
        <Box pl={'32px'}>
          {role === 'associate' ? (
            <>
              <Typography mt={0.5}>
                {t('sideBar.name')}:{' '}
                {capitalizeWords(firstName + ' ' + lastName)}
              </Typography>
              <Typography mt={0.3}>
                {t('sideBar.email')}: {email}
              </Typography>
              <Typography mt={0.3}>NIF: {nif}</Typography>
              <Typography mt={0.3}>
                {t('sideBar.associate')}: {id}
              </Typography>
            </>
          ) : (
            <Typography mt={0.3}>
              {t('sideBar.email')}: {email}
            </Typography>
          )}
        </Box>
        <Box sx={{ display: 'flex', justifyContent: 'center', pt: '10px' }}>
          <Typography pr="10px">{t('sideBar.logout')}</Typography>
          <IconButton
            component={Link}
            to="/sign-in"
            onClick={logOut}
            sx={{ p: 0 }}
          >
            <LogoutIcon />
          </IconButton>
        </Box>
      </Stack>
    </Box>
  )
}

const SideButton = ({ url, name, icon, selected }) => {
  const { t } = useTranslation()
  return (
    <ListItemButton component={Link} selected={selected} to={url}>
      <ListItemIcon>{icon}</ListItemIcon>
      <ListItemText primary={t(name)}></ListItemText>
    </ListItemButton>
  )
}

export default Side
