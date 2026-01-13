import React from 'react'
import {
  TableContainer,
  Table,
  TableHead,
  TableBody,
  TableRow,
  TableCell,
  Paper,
  useTheme,
  Box,
} from '@mui/material'

const StyledTableCell = ({ variant, children, sx, ...other }) => {
  const { palette } = useTheme()

  const cellSX = {
    head: {
      backgroundColor:
        palette.mode === 'dark' ? palette.primary[800] : palette.grey[200],
      color: palette.common.darkGrey,
      fontSize: '16px',
      fontWeight: 600,
      px: 6,
      height: '54px',
    },
    body: {
      backgroundImage: palette.bgApp.bgImage,
      minWidth: 'fit-content',
      px: 6,
      height: '54px',
    },
  }

  switch (variant) {
    case 'head':
      return (
        <TableCell sx={{ ...cellSX.head, ...sx }} {...other}>
          {children}
        </TableCell>
      )
    case 'body':
      return (
        <TableCell sx={{ ...cellSX.body, ...sx }} {...other}>
          {children}
        </TableCell>
      )
    case 'pagination':
      return (
        <Box
          sx={{
            ...cellSX.body,
            border: 0,
            ...sx,
          }}
          {...other}
        >
          {children}
        </Box>
      )
    default:
      return <TableCell {...other}>{children}</TableCell>
  }
}

export function CustomTable({ columns, rows, pagination }) {
  return (
    <Paper elevation={3} sx={{ borderRadius: 3, overflow: 'hidden' }}>
      <TableContainer>
        <Table aria-label="Historic" stickyHeader size="small">
          <TableHead>
            <TableRow>
              {columns.map((column) => (
                <StyledTableCell
                  key={column.id}
                  variant="head"
                  align={column.align}
                  style={{ minWidth: column.minWidth }}
                >
                  {column.label}
                </StyledTableCell>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {rows.map((row) => (
              <TableRow
                key={row.id}
                sx={
                  !pagination && {
                    '&:last-child td,&:last-child th': { border: 0 },
                  }
                }
              >
                {columns.map((column) => {
                  const value = row[column.id]
                  return (
                    <StyledTableCell
                      key={column.id}
                      variant="body"
                      align={column.align}
                    >
                      {column.format ? column.format(value) : value}
                    </StyledTableCell>
                  )
                })}
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
      {pagination ? (
        <StyledTableCell variant="pagination">{pagination}</StyledTableCell>
      ) : null}
    </Paper>
  )
}
