import React from 'react'
import { Box, MenuItem, Select } from '@mui/material'
import { useTranslation } from 'react-i18next'
import { fallbackLng, supportedLanguages } from '../../lib/i18n'

export const Language = ({ ...other }) => {
  const {
    i18n: { changeLanguage, language },
  } = useTranslation()

  const handleInputChange = (newValue) => {
    changeLanguage(newValue)
  }

  return (
    <Select
      defaultValue={fallbackLng}
      value={language}
      onChange={(e) => handleInputChange(e.target.value)}
      sx={{
        borderRadius: '16px',
        ...other.sx,
      }}
      SelectDisplayProps={{
        style: {
          padding: '10px 35px 10px 20px',
        },
      }}
      renderValue={(value) =>
        value && (
          <Box sx={{ display: 'flex', gap: 1 }}>
            <Box
              sx={{
                height: '21px',
                width: '30px',
                display: 'flex',
                justifyContent: 'center',
                alignItems: 'center',
                overflow: 'hidden',
                borderRadius: '8px',
              }}
            >
              <img
                loading="lazy"
                height="23px"
                srcSet={`https://flagcdn.com/w40/${supportedLanguages[value].flagCode.toLowerCase()}.png 2x`}
                src={`https://flagcdn.com/w20/${supportedLanguages[value].flagCode.toLowerCase()}.png`}
                alt=""
              />
            </Box>
            {value.toUpperCase()}
          </Box>
        )
      }
    >
      {Object.entries(supportedLanguages).map(([key, value]) => (
        <MenuItem key={key} value={key} sx={{ display: 'flex', gap: 1 }}>
          <Box
            sx={{
              height: '21px',
              width: '30px',
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              overflow: 'hidden',
              borderRadius: '8px',
            }}
          >
            <img
              loading="lazy"
              height="23px"
              srcSet={`https://flagcdn.com/w40/${value.flagCode.toLowerCase()}.png 2x`}
              src={`https://flagcdn.com/w20/${value.flagCode.toLowerCase()}.png`}
              alt=""
            />
          </Box>
          {value.label} ({key.toUpperCase()})
        </MenuItem>
      ))}
    </Select>
  )
}
