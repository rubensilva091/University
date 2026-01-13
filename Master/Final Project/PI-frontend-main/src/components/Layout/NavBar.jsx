import React from 'react'
import { AppBar, Box } from '@mui/material'
import MenuIcon from '@mui/icons-material/Menu'
import logotipo from '../../icon/logotipo.svg'
import { Link } from 'react-router-dom'
import { Language } from '../Language/'
import { CustomButton } from '../CustomButton/index'

export const NavBar = ({ sideMenu, sideMenuToggle }) => {
  return (
    <AppBar
      position="sticky"
      color=""
      sx={{
        width: '100vw',
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <Box
        variant="regular"
        sx={{
          display: 'flex',
          flexDirection: 'row',
          gap: { xs: 2, sm: 4 },
          justifyContent: 'space-between',
          alignItems: 'center',
          px: { xs: 4, md: 8 },
          py: 2,
        }}
      >
        <Box
          sx={{
            display: 'flex',
            flexDirection: 'row',
            gap: { xs: 2, sm: 4 },
            alignItems: 'center',
          }}
        >
          <CustomButton
            styleVariant="toggle"
            onClick={sideMenuToggle}
            selected={sideMenu}
            variant={sideMenu ? 'contained' : undefined}
            sx={{
              display: { md: 'none' },
              px: 3,
              py: 2,
              minWidth: 'fit-content',
              borderRadius: 2,
            }}
          >
            <MenuIcon />
          </CustomButton>
          <Link to="/">
            <img src={logotipo} alt="image" height="50px" />
          </Link>
        </Box>
        <Language />
      </Box>
    </AppBar>
  )
}
