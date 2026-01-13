import React from 'react'
import { TextField } from '@mui/material'

const style = {
  InputProps: {
    borderRadius: '8px',
  },
  inputProps: {
    paddingInline: '24px',
    paddingBlock: '10px',
  },
  InputLabelProps: {
    paddingInline: '8px',
    paddingBlock: 0,
  },
}

export const CustomTextField = ({
  required,
  label,
  onValueChange,
  InputProps,
  inputProps,
  InputLabelProps,
  onChange,
  ...other
}) => {
  function handleChange(event) {
    if (onValueChange) {
      onValueChange(event.target.value)
    } else if (onChange) {
      onChange(event)
    }
  }

  return (
    <TextField
      fullWidth
      label={label && `${label}${required ? '*' : ''}`}
      variant="outlined"
      size="small"
      onChange={handleChange}
      InputProps={{
        ...InputProps,
        style:
          InputProps && InputProps.style
            ? {
                ...style.InputProps,
                ...InputProps.style,
              }
            : style.InputProps,
      }}
      inputProps={{
        ...inputProps,
        style:
          inputProps && inputProps.style
            ? {
                ...style.inputProps,
                ...inputProps.style,
              }
            : style.inputProps,
      }}
      InputLabelProps={{
        ...InputLabelProps,
        style:
          InputLabelProps && InputLabelProps.style
            ? {
                ...style.InputLabelProps,
                ...InputLabelProps.style,
              }
            : style.InputLabelProps,
      }}
      {...other}
    ></TextField>
  )
}
