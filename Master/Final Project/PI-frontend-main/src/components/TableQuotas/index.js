import React from 'react'
import {
  TableContainer,
  Table,
  TableHead,
  TableBody,
  TableRow,
  TableCell,
  Paper,
  IconButton,
} from '@mui/material'
import { useTranslation } from 'react-i18next'
import { styled } from '@mui/material/styles'
import { tableCellClasses } from '@mui/material/TableCell'
import LinkIcon from '@mui/icons-material/Link'

function TableQuotas({ data }) {
  const { t } = useTranslation()
  const StyledTableCell = styled(TableCell)(({ theme }) => ({
    [`&.${tableCellClasses.head}`]: {
      backgroundColor: '#8984f0',
      color: theme.palette.common.white,
      fontSize: 15,
      fontWeight: 'bold',
    },
    [`&.${tableCellClasses.body}`]: {
      fontSize: 14,
    },
  }))
  const StyledTableRow = styled(TableRow)(({ theme }) => ({
    '&:nth-of-type(odd)': {
      backgroundColor: theme.palette.action.hover,
    },
    // hide last border
    '&:last-child td, &:last-child th': {
      border: 0,
    },
  }))

  const formatDate = (date) => {
    if (date) {
      const slice = date.split('T')
      if (slice.length > 0) return slice[0]
    }
    return date
  }

  const formatInvoice = (invoice) => {
    if (invoice) {
      return invoice
    }
    return t('table.quotes.noInvoice')
  }

  return (
    <TableContainer component={Paper} sx={{ maxHeight: '50vh' }}>
      <Table aria-label="Historic" stickyHeader>
        <TableHead>
          <TableRow>
            <StyledTableCell>Id</StyledTableCell>
            <StyledTableCell>{t('table.quotes.value')} </StyledTableCell>
            <StyledTableCell>{t('table.quotes.date')}</StyledTableCell>
            <StyledTableCell>{t('table.quotes.finalDate')}</StyledTableCell>
            <StyledTableCell>{t('table.quotes.invoice')}</StyledTableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {data.map((row) => (
            <StyledTableRow
              key={row.id}
              sx={{ '&:last-child td,&:last-child th': { border: 0 } }}
            >
              <TableCell>{row.id}</TableCell>
              <TableCell>{row.price.toFixed(2)}€</TableCell>
              <TableCell>{formatDate(row.start_date)}</TableCell>
              <TableCell>{formatDate(row.end_date)}</TableCell>
              <TableCell>
                {row.invoice_file ? (
                  <IconButton
                    aria-label="invoice"
                    href={row.invoice_file}
                    target="_blank"
                  >
                    <LinkIcon />
                  </IconButton>
                ) : (
                  formatInvoice(row.invoice_file)
                )}
              </TableCell>
            </StyledTableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  )
}
export default TableQuotas
