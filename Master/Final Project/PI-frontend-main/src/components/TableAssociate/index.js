import React, { useState } from 'react'
import { styled } from '@mui/material/styles'
import TableCell, { tableCellClasses } from '@mui/material/TableCell'
import formatDate from '../../utils/utils'
import { useTranslation } from 'react-i18next'
import UploadIcon from '@mui/icons-material/Upload'
import LinkIcon from '@mui/icons-material/Link'
import EditIcon from '@mui/icons-material/Edit'
import {
  TableContainer,
  Table,
  TableHead,
  TableBody,
  TableRow,
  Paper,
  IconButton,
  DialogTitle,
  Dialog,
  DialogActions,
  Button,
  DialogContent,
  TextField,
  Box,
} from '@mui/material'

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

export default function TableAssociate({
  rows,
  style,
  styleSimple,
  handleSubmitInvoice,
}) {
  const { t } = useTranslation()
  const [subscriptionInvoice, setSubscriptionInvoice] = useState('')
  const [subscriptionID, setSubscriptionID] = useState('')
  const [invoiceOpen, setInvoiceOpen] = useState(false)
  const [edit, setEdit] = useState(false)
  const handleInvoiceClickOpen = (subId, edit) => {
    setSubscriptionID(subId)
    setEdit(edit)
    setInvoiceOpen(true)
  }
  const handleStopInvoice = () => {
    setInvoiceOpen(false)
  }

  return (
    <TableContainer component={Paper} sx={{ maxHeight: '50vh' }}>
      <Table aria-label="Historic of associate" stickyHeader>
        <TableHead>
          <TableRow>
            <StyledTableCell align="center">ID</StyledTableCell>
            <StyledTableCell align="center">
              {t('table.quotes.date')}
            </StyledTableCell>
            <StyledTableCell align="center">
              {t('table.quotes.finalDate')}
            </StyledTableCell>
            <StyledTableCell align="center">
              {t('table.quotes.value')}
            </StyledTableCell>
            <StyledTableCell align="center">
              {t('table.quotes.invoice')}
            </StyledTableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {rows.map((row) => (
            <TableRow key={row.id}>
              <StyledTableCell align="center">{row.id}</StyledTableCell>
              <StyledTableCell align="center">
                {formatDate(row.start_date)}
              </StyledTableCell>
              <StyledTableCell align="center">
                {formatDate(row.end_date)}
              </StyledTableCell>
              <StyledTableCell align="center">{row.price}€</StyledTableCell>
              <StyledTableCell align="center">
                {row.invoice_file ? (
                  <Box>
                    <IconButton
                      size="small"
                      aria-label="invoice"
                      href={row.invoice_file}
                      target="_blank"
                    >
                      <LinkIcon />
                    </IconButton>
                    <IconButton
                      size="small"
                      aria-label="addInvoice"
                      onClick={() => handleInvoiceClickOpen(row.id, true)}
                    >
                      <EditIcon />
                    </IconButton>
                  </Box>
                ) : (
                  <IconButton
                    aria-label="addInvoice"
                    onClick={() => handleInvoiceClickOpen(row.id, false)}
                  >
                    <UploadIcon />
                  </IconButton>
                )}
              </StyledTableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
      <Dialog
        open={invoiceOpen}
        onClose={handleStopInvoice}
        aria-labelledby="alert-dialog-title"
        aria-describedby="alert-dialog-description"
        sx={{ height: '80vh' }}
      >
        <DialogTitle>
          {edit ? t('associatesTable.edit') : 'Upload'}{' '}
          {t('table.quotes.invoice')}
        </DialogTitle>
        <DialogContent>
          <Box
            component="form"
            sx={{
              display: 'flex',
              flexDirection: 'column',
              m: 'auto',
              width: 'fit-content',
              height: 'fit-content',
            }}
          >
            <TextField
              id="url"
              label={t('associatesTable.invoiceUrl')}
              variant="outlined"
              fullWidth
              helperText={t('associatesTable.urlToInvoice')}
              sx={{
                mb: 2,
                mt: 2,
              }}
              onChange={(e) => setSubscriptionInvoice(e.target.value)}
            />
            <DialogActions>
              <Button
                variant="contained"
                size="small "
                style={styleSimple}
                onClick={() =>
                  handleSubmitInvoice(subscriptionID, subscriptionInvoice)
                }
              >
                {t('table.quotes.submit')}
              </Button>
              <Button
                onClick={handleStopInvoice}
                size="small "
                style={style}
                variant="contained"
              >
                {t('table.quotes.cancel')}
              </Button>
            </DialogActions>
          </Box>
        </DialogContent>
      </Dialog>
    </TableContainer>
  )
}
