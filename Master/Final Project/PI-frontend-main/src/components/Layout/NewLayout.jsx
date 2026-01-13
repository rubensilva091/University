import React, { useState } from 'react'
import { Outlet } from 'react-router-dom'
import { Box, Divider, Drawer } from '@mui/material'
import { NewPoweredBy } from '../PoweredBy/NewPoweredBy'
import { NavBar } from './NavBar'
import { NewSideBar } from '../Sidebar/NewSideBar'
import { useTheme } from '@emotion/react'

export const NewLayout = () => {
  const [sideMenu, setSideMenu] = useState(false)
  const { palette } = useTheme()

  const drawer = <NewSideBar />

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        height: '100vh',
        overflow: 'hidden',
        backgroundColor: palette.bgApp.bgColor,
        backgroundImage: palette.bgApp.bgImage,
      }}
    >
      <NavBar
        sideMenu={sideMenu}
        sideMenuToggle={() => setSideMenu(!sideMenu)}
      />
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'row',
          height: '100%',
        }}
      >
        <Drawer
          variant="temporary"
          open={sideMenu}
          onClose={() => setSideMenu(!sideMenu)}
          sx={{
            display: {
              xs: 'flex',
              md: 'none',
            },
          }}
          ModalProps={{ keepMounted: true }} // Better open performance on mobile.
        >
          <Box sx={{ width: 260, mt: '73px' }} />
          {drawer}
        </Drawer>
        <Box
          sx={{
            display: { xs: 'none', md: 'flex' },
            flexDirection: 'column',
            minWidth: 260,
            bgcolor: palette.background.drawer,
            justifyContent: 'space-between',
          }}
        >
          {drawer}
        </Box>
        <Divider
          orientation="vertical"
          sx={{
            display: { xs: 'none', md: 'flex' },
            borderRightWidth: '2px',
          }}
        />
        <Box
          sx={{ height: 'calc(100vh - 73px)', width: '100%', overflow: 'auto' }}
        >
          <Outlet />
        </Box>
      </Box>
      <NewPoweredBy />
    </Box>
  )
}
