import React, { useState } from 'react'
import {
  TableContainer,
  Table,
  TableHead,
  TableBody,
  TableRow,
  TableCell,
  Paper,
  FormControl,
  Select,
  MenuItem,
  IconButton,
} from '@mui/material'
import { Link } from 'react-router-dom'
import DeleteIcon from '@mui/icons-material/Delete'
import AddCircleOutlineIcon from '@mui/icons-material/AddCircleOutline'
import { capitalizeWords } from '../../utils/utils'
import { useTranslation } from 'react-i18next'
import { styled } from '@mui/material/styles'
import { tableCellClasses } from '@mui/material/TableCell'
import StatusDialog from '../StatusDialog'
import RestoreFromTrashIcon from '@mui/icons-material/RestoreFromTrash'
import DownloadIcon from '@mui/icons-material/Download'

function TableAssociates({
  data,
  status,
  subStatus,
  onChange,
  onDisable,
  onVerify,
  handleDownloadAssociate,
}) {
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

  if (!status) status = 5
  if (!subStatus) subStatus = 4
  const [accountToDisable, setAccountToDisable] = useState('')
  const [accountToVerify, setAccountToVerify] = useState('')
  const { t } = useTranslation()
  const [disableOpen, setDisableOpen] = useState(false)
  const [verifyOpen, setVerifyOpen] = useState(false)

  const handleDisableClickOpen = (account) => {
    setAccountToDisable(account)
    setDisableOpen(true)
  }
  const handleVerifyClickOpen = (account) => {
    setAccountToVerify(account)
    setVerifyOpen(true)
  }

  const handleClose = () => {
    setDisableOpen(false)
    setVerifyOpen(false)
  }

  const Status = {
    1: t('associatesTable.status.pending'),
    2: t('associatesTable.status.verified'),
    3: t('associatesTable.status.disabled'),
    4: t('associatesTable.status.other'),
    5: t('associatesTable.status.all'),
  }

  const SubStatus = {
    Expired: t('associatesTable.subStatus.expired'),
    Valid: t('associatesTable.subStatus.valid'),
  }

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
    if (date === '0001-01-01T00:00:00Z')
      return t('associatesTable.noSubscription')
    if (date) {
      const slice = date.split('T')
      if (slice.length > 0) return slice[0]
    }

    return date
  }

  const formatStatus = (status) => {
    status = Status[status]
    if (status) return status
    return 'Unknonwn'
  }

  const formatSub = (status, valid) => {
    if (valid) {
      return SubStatus[status]
    }
    return t('associatesTable.inexistent')
  }

  const handleDisableAfirmation = (account) => {
    onDisable(account)
    handleClose()
  }

  const handleVerifyAfirmation = (account) => {
    onVerify(account)
    handleClose()
  }

  return (
    <TableContainer component={Paper} sx={{ maxHeight: '100vh' }}>
      <Table aria-label="Historic" stickyHeader>
        <TableHead>
          <TableRow>
            <StyledTableCell width="10">Id</StyledTableCell>
            <StyledTableCell width="250">
              {t('associatesTable.associate.email')}{' '}
            </StyledTableCell>
            <StyledTableCell width="130">
              {t('associatesTable.associate.name')}
            </StyledTableCell>
            <StyledTableCell width="150">
              <FormControl variant="standard">
                <Select
                  sx={{
                    color: 'white',
                    fontSize: 15,
                    fontWeight: 'bold',
                  }}
                  autoWidth
                  value={subStatus}
                  label="SubStatus"
                  onChange={(e) =>
                    onChange(
                      e.target.value < 4
                        ? (prevState) => ({
                            ...prevState,
                            page: 1,
                            subStatus: e.target.value,
                          })
                        : (prevState) => ({
                            ...prevState,
                            page: 1,
                            subStatus: null,
                          }),
                    )
                  }
                >
                  <MenuItem value={1}>
                    {t('associatesTable.subStatus.expired')}
                  </MenuItem>
                  <MenuItem value={2}>
                    {t('associatesTable.subStatus.valid')}
                  </MenuItem>
                  <MenuItem value={3}>
                    {t('associatesTable.inexistent')}
                  </MenuItem>
                  <MenuItem value={4}>
                    {t('associatesTable.status.all')}
                  </MenuItem>
                </Select>
              </FormControl>
            </StyledTableCell>
            <StyledTableCell width="240">
              {t('associatesTable.associate.endDate')}
            </StyledTableCell>
            <StyledTableCell width="150">
              <FormControl variant="standard">
                <Select
                  sx={{
                    color: 'white',
                    fontSize: 15,
                    fontWeight: 'bold',
                  }}
                  autoWidth
                  value={status}
                  label="Status"
                  onChange={(e) =>
                    onChange(
                      e.target.value < 5
                        ? (prevState) => ({
                            ...prevState,
                            page: 1,
                            status: e.target.value,
                          })
                        : (prevState) => ({
                            ...prevState,
                            page: 1,
                            status: null,
                          }),
                    )
                  }
                >
                  <MenuItem value={1}>{Status[1]}</MenuItem>
                  <MenuItem value={2}>{Status[2]}</MenuItem>
                  <MenuItem value={3}>{Status[3]}</MenuItem>
                  <MenuItem value={4}>{Status[4]}</MenuItem>
                  <MenuItem value={5}>{Status[5]}</MenuItem>
                </Select>
              </FormControl>
            </StyledTableCell>
            <StyledTableCell>
              {t('associatesTable.associate.userDetails')}
            </StyledTableCell>
            <StyledTableCell>
              {t('associatesTable.associate.disableAccount')}
            </StyledTableCell>
            <StyledTableCell>Extract</StyledTableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {data ? (
            data.map((row) => (
              <StyledTableRow
                key={row.id}
                sx={{ '&:last-child td,&:last-child th': { border: 0 } }}
              >
                <TableCell>{row.id}</TableCell>
                <TableCell>{row.email}</TableCell>
                <TableCell>
                  {capitalizeWords(row.first_name + ' ' + row.last_name)}
                </TableCell>
                <TableCell>
                  {formatSub(
                    row.subscription_status,
                    row.subscription_end_date.Valid,
                  )}
                </TableCell>
                <TableCell>
                  {formatDate(row.subscription_end_date.Time)}
                </TableCell>
                <TableCell>{formatStatus(row.status_id)}</TableCell>
                <TableCell>
                  <IconButton
                    aria-label="details"
                    component={Link}
                    to={'/associates/account?email=' + row.email}
                  >
                    <AddCircleOutlineIcon />
                  </IconButton>
                </TableCell>
                <TableCell>
                  {row.status_id === 2 ? (
                    <IconButton
                      aria-label="delete"
                      disabled={row.status_id === 3}
                      onClick={() => handleDisableClickOpen(row.email)}
                    >
                      <DeleteIcon />
                    </IconButton>
                  ) : (
                    <IconButton
                      aria-label="verify"
                      disabled={row.status_id === 2}
                      onClick={() => handleVerifyClickOpen(row.email)}
                    >
                      <RestoreFromTrashIcon />
                    </IconButton>
                  )}
                </TableCell>
                <TableCell>
                  <IconButton
                    onClick={() => handleDownloadAssociate(row.email)}
                  >
                    <DownloadIcon />
                  </IconButton>
                </TableCell>
              </StyledTableRow>
            ))
          ) : (
            <></>
          )}
        </TableBody>
      </Table>
      <StatusDialog
        open={disableOpen}
        handleClose={handleClose}
        account={accountToDisable}
        styleSimple={styleSimple}
        style={style}
        handleAfirmation={handleDisableAfirmation}
        disable={true}
      ></StatusDialog>
      <StatusDialog
        open={verifyOpen}
        handleClose={handleClose}
        account={accountToVerify}
        styleSimple={styleSimple}
        style={style}
        handleAfirmation={handleVerifyAfirmation}
        disable={false}
      ></StatusDialog>
    </TableContainer>
  )
}
export default TableAssociates
