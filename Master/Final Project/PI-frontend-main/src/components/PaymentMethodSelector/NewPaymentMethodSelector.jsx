import React from 'react'
import mbWay from '../../icon/mbWay.svg'
import mb from '../../icon/mb.svg'
import { Box, Tab, Tabs } from '@mui/material'

export const NewPaymentMethodSelector = ({ value, onChange }) => {
  function a11yProps(index) {
    return {
      id: `payment-method-tab-${index}`,
      'aria-controls': `payment-method-tabpanel-${index}`,
      sx: {
        height: '60px',
        paddingBlock: 3,
        paddingInline: 5,
      },
    }
  }

  const handleChange = (_, newTab) => {
    onChange(newTab)
  }

  return (
    <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
      <Tabs value={value} onChange={handleChange}>
        <Tab
          value={'mbway'}
          label={<img src={mbWay} alt="image" height="100%" />}
          {...a11yProps(0)}
        />
        <Tab
          value={'mb'}
          label={<img src={mb} alt="image" height="100%" />}
          {...a11yProps(1)}
        />
      </Tabs>
    </Box>
  )
}
