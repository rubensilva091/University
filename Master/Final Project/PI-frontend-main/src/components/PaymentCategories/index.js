import React from 'react'
import Box from '@mui/material/Box'
import InputLabel from '@mui/material/InputLabel'
import MenuItem from '@mui/material/MenuItem'
import FormControl from '@mui/material/FormControl'
import Select from '@mui/material/Select'
import InputAdornment from '@mui/material/InputAdornment'
import { useTranslation } from 'react-i18next'
import { Button } from '@mui/material'
import { NumberTextField, selectTextFromTo } from '../NumberTextField'

export function PaymentCategories({ categories, submitCategory }) {
  const [category, setCategory] = React.useState('')
  const [price, setPrice] = React.useState('0.00')
  const [period, setPeriod] = React.useState('')
  const { t } = useTranslation()
  const handleChange = (event) => {
    setCategory(event.target.value)
  }
  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,
    marginLeft: 10,
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  const handlePriceChange = (e) => {
    const value = e.target.value
    const numberRegex = /^\d*(?:\.\d{0,2})?$/

    if (numberRegex.test(value)) {
      setPrice(value)
    } else {
      const separatorRegex = /[,.][,.]/
      if (separatorRegex.test(value)) {
        const separatorPosition = price.indexOf('.')
        selectTextFromTo('price-textfield', separatorPosition + 1, price.length)
      } else {
        const incorrectCharRegex = /[^\d.]/
        const incorrectChar = value.search(incorrectCharRegex)
        selectTextFromTo('price-textfield', incorrectChar, incorrectChar)
      }
    }
  }

  const handlePriceFocus = () => {
    const separatorPosition = price.indexOf('.')
    selectTextFromTo('price-textfield', 0, separatorPosition)
  }

  return (
    <Box sx={{ minWidth: 120 }}>
      <FormControl sx={{ m: 1, width: '25ch' }} size="medium">
        <InputLabel id="demo-simple-select-label">
          {t('paymentCategories.memberCategory')}
        </InputLabel>
        <Select
          labelId="demo-simple-select-label"
          id="demo-simple-select"
          value={category}
          label="memberCategory"
          onChange={handleChange}
        >
          {categories?.map((name, index) => {
            return (
              <MenuItem key={index} value={name}>
                {name}
              </MenuItem>
            )
          })}
        </Select>
      </FormControl>
      <FormControl sx={{ m: 1, width: '25ch' }} size="medium">
        <NumberTextField
          id="period-textfield"
          label={t('paymentCategories.period')}
          InputProps={{
            endAdornment: (
              <InputAdornment position="start">
                {t('paymentCategories.months')}
              </InputAdornment>
            ),
          }}
          value={period}
          setNumber={setPeriod}
        />
      </FormControl>
      <Box
        component="form"
        sx={{
          '& > :not(style)': { m: 1, width: '25ch' },
        }}
        noValidate
        autoComplete="off"
      >
        <NumberTextField
          id="price-textfield"
          label={t('paymentCategories.value')}
          InputProps={{
            endAdornment: <InputAdornment position="start">€</InputAdornment>,
          }}
          value={price}
          onChange={(e) => handlePriceChange(e)}
          onFocus={() => handlePriceFocus()}
        />
      </Box>
      <Button
        type="submit"
        size="medium"
        variant="contained"
        style={style}
        onClick={() =>
          submitCategory(category, parseFloat(price), parseInt(period))
        }
      >
        {t('paymentSettings.button')}
      </Button>
    </Box>
  )
}
