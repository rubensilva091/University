import React from 'react'
import TextField from '@mui/material/TextField'

export const NumberTextField = ({
  id,
  setNumber,
  value,
  error,
  helperText,
  label,
  InputProps,
  onChange,
  onFocus,
  required,
}) => {
  const handleChange = (e) => {
    const regex = /^\d+$/
    if (e.target.value === '' || regex.test(e.target.value)) {
      setNumber(e.target.value)
    }
  }
  return (
    <TextField
      id={id}
      error={error}
      helperText={helperText}
      value={value}
      onChange={(e) => (onChange ? onChange(e) : handleChange(e))}
      onFocus={onFocus}
      variant="outlined"
      label={label}
      InputProps={InputProps}
      required={required}
    ></TextField>
  )
}

export const selectTextFromTo = async (id, from, to) => {
  const element = document.getElementById(id)
  element.focus()
  await new Promise((resolve) => setTimeout(resolve, 0))
  element.setSelectionRange(from, to)
}
