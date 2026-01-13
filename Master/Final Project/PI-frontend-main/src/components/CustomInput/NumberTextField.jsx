import React from 'react'
import { NumericFormat } from 'react-number-format'
import { CustomTextField } from './CustomTextField'

export const NumberTextField = ({
  label,
  value,
  onValueChange,
  decimalScale,
  ...other
}) => {
  return (
    <NumericFormat
      label={label}
      customInput={CustomTextField}
      decimalScale={decimalScale ?? undefined}
      value={value}
      type="text"
      onValueChange={({ value: v }) => {
        onValueChange(v)
      }}
      {...other}
    />
  )
}
