import React from 'react'
import { Avatar } from '@mui/material'

export const CustomAvatar = ({ name = '', sx, children, ...other }) => {
  function stringToColor(string) {
    let hash = 0

    for (let i = 0; i < string.length; i += 1) {
      hash = string.charCodeAt(i) + ((hash << 5) - hash)
    }

    let color = '#'

    for (let i = 0; i < 3; i += 1) {
      const value = (hash >> (i * 8)) & 0xff
      color += `00${value.toString(16)}`.slice(-2)
    }

    return color
  }

  function userNameToAvatar(name) {
    if (children) {
      return children
    } else {
      if (name) {
        const names = name.split(' ')
        return {
          sx: {
            bgcolor: stringToColor(name),
            ...sx,
          },
          children: `${names[0][0]}${names[names.length - 1][0]}`,
        }
      }
    }
  }

  return <Avatar {...other} {...userNameToAvatar(name)} />
}
