import React, { useState } from 'react'
import { styled } from '@mui/material/styles'
import Table from '@mui/material/Table'
import TableBody from '@mui/material/TableBody'
import TableCell, { tableCellClasses } from '@mui/material/TableCell'
import TableContainer from '@mui/material/TableContainer'
import TableHead from '@mui/material/TableHead'
import TableRow from '@mui/material/TableRow'
import Paper from '@mui/material/Paper'
import { useTranslation } from 'react-i18next'
import DeleteIcon from '@mui/icons-material/Delete'
import IconButton from '@mui/material/IconButton'
import { Button, Dialog, DialogTitle, DialogActions } from '@mui/material'

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

const style = {
  background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
  borderRadius: 3,
  border: 0,
  color: 'white',
  height: 48,
  marginLeft: '10px',
  // margin: "30px  20px",
  padding: '0 30px',
  boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
}

const styleSimple = {
  background: 'white',
  borderRadius: 3,
  border: 0,
  color: '#8984f0',
  height: 48,
  // margin: "30px  20px",
  padding: '0 30px',
  boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
}

function TablePayments({ paymentOptions, deleteOption }) {
  const { t } = useTranslation()
  const [category, setCategory] = useState(undefined)
  const [period, setPeriod] = useState(undefined)
  const [open, setOpen] = useState(false)

  return (
    <>
      <TableContainer component={Paper} sx={{ maxHeight: '50vh' }}>
        <Table aria-label="Table of payments" stickyHeader>
          <TableHead>
            <TableRow>
              <StyledTableCell>{t('paymentsTable.category')}</StyledTableCell>
              <StyledTableCell>{t('paymentsTable.period')}</StyledTableCell>
              <StyledTableCell>{t('paymentsTable.price')}</StyledTableCell>
              <StyledTableCell>{t('paymentsTable.delete')}</StyledTableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {paymentOptions?.map((row, index) => (
              <StyledTableRow key={index}>
                <StyledTableCell>{row.category}</StyledTableCell>
                <StyledTableCell>
                  {row.period} {t('paymentCategories.months')}
                </StyledTableCell>
                <StyledTableCell>{row.price} €</StyledTableCell>
                <StyledTableCell>
                  <IconButton
                    aria-label="delete"
                    onClick={() => {
                      setOpen(true)
                      setCategory(row.category)
                      setPeriod(row.period)
                    }}
                  >
                    <DeleteIcon />
                  </IconButton>
                </StyledTableCell>
              </StyledTableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
      <Dialog
        open={open}
        onClose={() => setOpen(false)}
        aria-labelledby="alert-dialog-title"
        aria-describedby="alert-dialog-description"
        sx={{ height: '100vh' }}
      >
        <DialogTitle id="alert-dialog-title" sx={{ mt: '50px' }}>
          {t('associatesTable.alert.deleteConfirm')}
        </DialogTitle>
        <DialogActions sx={{ mb: '50px' }}>
          <Button
            onClick={() => setOpen(false)}
            variant="contained"
            to={'/sign-in'}
            size="medium "
            style={styleSimple}
          >
            {t('associatesTable.alert.no')}
          </Button>
          <Button
            onClick={() => {
              console.log(category, period)
              deleteOption(category, parseInt(period))
              setOpen(false)
            }}
            variant="contained"
            sx={{ mr: '24px' }}
            size="medium "
            style={style}
          >
            {' '}
            {t('associatesTable.alert.yes')}
          </Button>
        </DialogActions>
      </Dialog>
    </>
  )
}
export default TablePayments
