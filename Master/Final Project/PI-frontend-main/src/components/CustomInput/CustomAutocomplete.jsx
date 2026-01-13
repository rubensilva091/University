import React from 'react'
import { Autocomplete } from '@mui/material'
import { CustomTextField } from './CustomTextField'

export const CustomAutocomplete = ({
  required,
  label,
  onValueChange,
  startAdornment,
  endAdornment,
  InputProps,
  inputProps,
  InputLabelProps,
  renderOption,
  error,
  helperText,
  value,
  ...other
}) => {
  function handleChange(_, value) {
    if (onValueChange) {
      onValueChange(value)
    }
  }

  return (
    <Autocomplete
      {...other}
      autoHighlight
      value={value !== '' ? value : null}
      onChange={handleChange}
      getOptionLabel={(option) => option}
      renderOption={renderOption}
      renderInput={(params) => (
        <CustomTextField
          {...params}
          required={required}
          error={error}
          helperText={helperText}
          autoComplete="off"
          size="small"
          label={label}
          InputProps={{
            ...params.InputProps,
            style: {
              borderRadius: '8px',
              padding: 0,
              paddingInlineStart: '20px',
              ...(InputProps && 'style' in InputProps
                ? InputProps.style
                : undefined),
            },
            startAdornment: startAdornment,
            endAdornment: endAdornment ?? params.InputProps.endAdornment,
            ...InputProps,
          }}
          inputProps={{
            ...params.inputProps,
            autoComplete: 'off',
            style: {
              padding: '10px 60px 10px 0px',
              width: '100%',
              ...(inputProps && 'style' in inputProps
                ? inputProps.style
                : undefined),
            },
            ...inputProps,
          }}
          InputLabelProps={{
            style: {
              paddingInlineStart: '8px',
              py: 0,
              ...(InputLabelProps && 'style' in InputLabelProps
                ? InputLabelProps.style
                : undefined),
            },
            ...InputLabelProps,
          }}
        />
      )}
    />
  )
}
