import React, { useEffect, useState } from 'react'
import {
  Box,
  TextField,
  InputAdornment,
  Autocomplete,
  FormHelperText,
  FormControl,
} from '@mui/material'
import { countries, defaultCountryCode } from '../../utils/countries'
import { NumberTextField } from './NumberTextField'

export const PhoneNumberTextField = ({
  onValueChange,
  value,
  selectedCountryCode,
  error,
  helperText,
  label,
  ...other
}) => {
  useEffect(() => {
    let countryCode
    if (value) {
      const phone = value.match(/\(([^)]+)\)/g)[0].replace(/\(|\)/g, '')
      countryCode = Object.entries(countries).find(
        ([, info]) => info.phone === phone,
      )
    }
    setCountryCode(
      countryCode ? countryCode[0] : selectedCountryCode ?? defaultCountryCode,
    )
  }, [])

  const [countryCode, setCountryCode] = useState('')
  const [number, setNumber] = useState(
    value ? value.replace(/\([^)]*\) ?/, '') : '',
  )

  const handleInputChange = (newValue) => {
    if (countries[newValue]) {
      setCountryCode(newValue)
      onValueChange(`(${countries[newValue].phone}) ${number}`)
    }
  }
  if (countryCode) {
    return (
      <FormControl
        sx={{
          display: 'flex',
          gap: 0,
        }}
      >
        <Box
          sx={{
            display: 'flex',
          }}
          aria-describedby="my-helper-text"
          {...other}
        >
          <Autocomplete
            options={Object.keys(countries)}
            autoHighlight
            defaultValue={countryCode}
            getOptionLabel={(option) => option}
            onChange={(_, newValue) => handleInputChange(newValue)}
            disableCloseOnSelect
            disableClearable
            slotProps={{
              paper: {
                sx: {
                  width: '200px',
                },
              },
            }}
            renderOption={(props, option) => (
              <Box
                component="li"
                sx={{ '& > img': { mr: 2, flexShrink: 0 } }}
                {...props}
              >
                <img
                  loading="lazy"
                  width="25"
                  srcSet={`https://flagcdn.com/w40/${option.toLowerCase()}.png 2x`}
                  src={`https://flagcdn.com/w20/${option.toLowerCase()}.png`}
                  alt=""
                />
                {countries[option].label} ({option.toUpperCase()})
              </Box>
            )}
            renderInput={(params) => (
              <TextField
                {...params}
                error={error}
                InputProps={{
                  ...params.InputProps,
                  style: {
                    borderRadius: '8px',
                    borderStartEndRadius: '0px',
                    borderEndEndRadius: '0px',
                    padding: 0,
                    paddingInlineStart: '20px',
                    width: 'max-content',
                  },
                  startAdornment: (
                    <img
                      loading="lazy"
                      width="25"
                      srcSet={`https://flagcdn.com/w40/${countryCode.toLowerCase()}.png 2x`}
                      src={`https://flagcdn.com/w20/${countryCode.toLowerCase()}.png`}
                      alt=""
                    />
                  ),
                  endAdornment: <></>,
                }}
                inputProps={{
                  ...params.inputProps,
                  autoComplete: 'tel',
                  style: {
                    textTransform: 'uppercase',
                    padding: '10px 5px 10px 10px',
                  },
                }}
              />
            )}
          />
          <NumberTextField
            label={label}
            value={number}
            error={error}
            onValueChange={(number) => {
              setNumber(number)
              onValueChange(`(${countries[countryCode].phone}) ${number}`)
            }}
            decimalScale={0}
            allowNegative={false}
            InputProps={{
              style: {
                borderRadius: '8px',
                borderEndStartRadius: 0,
                borderStartStartRadius: 0,
              },
              startAdornment: (
                <InputAdornment position="start">
                  ({countries[countryCode].phone})
                </InputAdornment>
              ),
            }}
            inputProps={{
              style: {
                padding: '10px',
                paddingInlineStart: 0,
                paddingInlineEnd: '24px',
              },
            }}
          />
        </Box>
        <FormHelperText
          id="my-helper-text"
          error={error}
          sx={{ marginTop: error ? '3px' : 0 }}
        >
          {helperText}
        </FormHelperText>
      </FormControl>
    )
  }
}
