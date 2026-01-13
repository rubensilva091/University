import React from 'react'
import {
  Box,
  FormControl,
  FormHelperText,
  InputLabel,
  MenuItem,
  Select,
} from '@mui/material'

export const CustomSelect = ({
  onValueChange,
  label,
  options,
  renderOption,
  renderValue,
  sx,
  error,
  helperText,
  ...other
}) => {
  return (
    <FormControl fullWidth>
      <InputLabel
        size="small"
        style={{
          paddingInlineStart: '8px',
          py: 0,
        }}
        error={error}
      >
        {label}
      </InputLabel>
      <Select
        label={label}
        onChange={(e) => onValueChange && onValueChange(e.target.value)}
        sx={{
          borderRadius: '8px',
          width: '100%',
          ...sx,
        }}
        SelectDisplayProps={{
          style: {
            padding: '10px 35px 10px 20px',
          },
        }}
        renderValue={renderValue ? (value) => renderValue(value) : undefined}
        error={error}
        {...other}
      >
        {options.map((value, index) => {
          return (
            <MenuItem key={index} value={value}>
              {renderOption(value)}
            </MenuItem>
          )
        })}
      </Select>
      <FormHelperText error={error}>{helperText}</FormHelperText>
    </FormControl>
  )
}
