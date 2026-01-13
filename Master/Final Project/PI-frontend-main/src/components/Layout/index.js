import { useTranslation } from 'react-i18next'
import * as React from 'react'
import AppBar from '@mui/material/AppBar'
import Box from '@mui/material/Box'
import Drawer from '@mui/material/Drawer'
import IconButton from '@mui/material/IconButton'
import MenuIcon from '@mui/icons-material/Menu'
import Toolbar from '@mui/material/Toolbar'
import { Outlet, Link } from 'react-router-dom'
import FormControl from '@mui/material/FormControl'
import Side from '../Sidebar'
import Select from '@mui/material/Select'
import MenuItem from '@mui/material/MenuItem'
import logotipo from '../../icon/logotipo.svg'
import { useTheme } from '@emotion/react'
import { PoweredBy } from '../PoweredBy'

const drawerWidth = 260

function Layout(props) {
  const { window } = props
  const [mobileOpen, setMobileOpen] = React.useState(false)
  const languageStored = localStorage.getItem('i18nextLng')
  const theme = useTheme()

  const handleDrawerToggle = () => {
    setMobileOpen(!mobileOpen)
  }

  const {
    t,
    i18n: { changeLanguage, language },
  } = useTranslation()

  const handleChangeLanguage = () => {
    const newLanguage = language === 'en' ? 'pt' : 'en'
    changeLanguage(newLanguage)
  }

  const drawer = <Side></Side>

  const container =
    window !== undefined ? () => window().document.body : undefined

  return (
    <Box sx={{ display: 'flex' }}>
      <AppBar
        position="fixed"
        sx={{
          width: { sm: `calc(100% - ${drawerWidth}px)` },
          ml: { sm: `${drawerWidth}px` },
          boxShadow: 0,
          backgroundColor: 'white',
          borderBottomColor: theme.palette.divider,
          borderBottomWidth: 1,
          borderBottomStyle: 'solid',
        }}
      >
        <Toolbar
          variant="regular"
          sx={{
            justifyContent: 'space-between',
          }}
        >
          <IconButton
            aria-label="open drawer"
            edge="start"
            onClick={handleDrawerToggle}
            sx={{ mr: 2, display: { sm: 'none' }, color: '#8984f0' }}
          >
            <MenuIcon />
          </IconButton>
          <Link to="/">
            <img src={logotipo} alt="image" height="50px" />
          </Link>

          <FormControl size="small">
            <Select value={languageStored} onChange={handleChangeLanguage}>
              <MenuItem value={'pt'}>{t('language.portuguese')}</MenuItem>
              <MenuItem value={'en'}>{t('language.english')}</MenuItem>
            </Select>
          </FormControl>
        </Toolbar>
      </AppBar>
      <Box
        component="nav"
        sx={{ width: { sm: drawerWidth }, flexShrink: { sm: 0 } }}
        aria-label="mailbox folders"
      >
        <Drawer
          container={container}
          variant="temporary"
          open={mobileOpen}
          onClose={handleDrawerToggle}
          ModalProps={{
            keepMounted: true, // Better open performance on mobile.
          }}
          sx={{
            display: { xs: 'block', sm: 'none' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
            },
          }}
        >
          {drawer}
        </Drawer>
        <Drawer
          variant="permanent"
          sx={{
            display: { xs: 'none', sm: 'block' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
            },
          }}
          open
        >
          {drawer}
        </Drawer>
      </Box>
      <Box
        component="main"
        sx={{
          flexGrow: 1,
          p: 3,
          width: { sm: `calc(100% - ${drawerWidth}px)` },
        }}
      >
        <Toolbar />
        <Outlet></Outlet>
      </Box>
      <PoweredBy />
    </Box>
  )
}

export default Layout
