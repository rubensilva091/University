import Box from '@mui/material/Box'
import TextField from '@mui/material/TextField'
import {
  Typography,
  Grid,
  MenuItem,
  Dialog,
  Button,
  DialogActions,
  DialogTitle,
} from '@mui/material'
import TableAssociate from '../TableAssociate'
import { useTranslation } from 'react-i18next'
import { capitalizeWords } from '../../utils/utils'
import InputAdornment from '@mui/material/InputAdornment'
import EditIcon from '@mui/icons-material/Edit'
import React, { useEffect, useState } from 'react'

function DataAssociate({
  associateData,
  email,
  editProfile,
  extractAssociate,
  handleSubmitInvoice,
}) {
  const addButtonStyle = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 30,
    width: 100,
    marginLeft: '10px',
    padding: '0 30px',
    marginTop: '4px',
  }
  const { t } = useTranslation()
  const { profile, subscriptions } = associateData
  const loyalty = profile.createdAt.split('T')
  const [firstName, setFirstName] = useState(profile.firstName)
  const [lastName, setLastName] = useState(profile.lastName)
  const [tlm, setTLM] = useState(profile.tlm)
  const [nif, setNIF] = useState(profile.nif)
  const [role, setRole] = useState(profile.role)
  const [openRole, setOpenRole] = useState(false)
  const [open, setOpen] = useState(false)
  const [changeRole, setChangeRole] = useState(false)
  const roles = [
    {
      value: 'associate',
      label: 'Associate',
    },
    {
      value: 'admin',
      label: 'Administrator',
    },
  ]
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

  const handleClickOpen = (newRole) => {
    setRole(newRole)
    setOpenRole(true)
  }
  const handleClose = () => {
    setRole(profile.role)
    setOpenRole(false)
  }
  const handleRoleAfirmation = () => {
    setChangeRole(true)
    setOpenRole(false)
  }

  const handleEditAfirmation = () => {
    if (changeRole) {
      editProfile((prevState) => ({
        ...prevState,
        role,
        firstName,
        lastName,
        tlm,
        nif,
      }))
    } else {
      editProfile((prevState) => ({
        ...prevState,
        firstName,
        lastName,
        tlm,
        nif,
      }))
    }
    setChangeRole(false)
    setOpen(false)
  }

  useEffect(() => {
    if (associateData) {
      setRole(associateData.profile.role)
    }
  }, [associateData])

  return (
    <Box>
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
        }}
      >
        <Typography color="#8984f0" fontSize="1.5rem" mb="20px">
          {t('associateData.header')}
        </Typography>
        <Button
          variant="contained"
          style={addButtonStyle}
          size="small"
          onClick={extractAssociate}
        >
          {t('associatesTable.associate.export')}
        </Button>
      </Box>
      <Grid
        container
        spacing={2}
        direction="row"
        justifyContent="flex-start"
        alignItems="center"
      >
        <Grid item xs={2}>
          <TextField
            id="outlined-basic"
            label={t('associateData.associate')}
            defaultValue={profile.ID}
            variant="outlined"
            sx={{ width: '100%', mb: '10px' }}
            InputProps={{
              readOnly: true,
            }}
          />
        </Grid>
        <Grid item xs={4}>
          <TextField
            id="input-with-icon-textfield"
            label={t('associateData.name')}
            sx={{ width: '100%', mb: '10px' }}
            defaultValue={capitalizeWords(profile.firstName)}
            onChange={(e) => setFirstName(e.target.value)}
            InputProps={{
              endAdornment: (
                <InputAdornment position="end">
                  <EditIcon></EditIcon>
                </InputAdornment>
              ),
            }}
            variant="outlined"
          />
        </Grid>
        <Grid item xs={4}>
          <TextField
            id="input-with-icon-textfield"
            label={t('register.lastName')}
            sx={{ width: '100%', mb: '10px' }}
            defaultValue={capitalizeWords(profile.lastName)}
            onChange={(e) => setLastName(e.target.value)}
            InputProps={{
              endAdornment: (
                <InputAdornment position="end">
                  <EditIcon></EditIcon>
                </InputAdornment>
              ),
            }}
            variant="outlined"
          />
        </Grid>
        <Grid item xs={2}>
          <TextField
            id="outlined-basic"
            label={t('associateData.dateOfLoyalty')}
            defaultValue={loyalty[0]}
            variant="outlined"
            sx={{ width: '100%', mb: '10px' }}
            InputProps={{
              readOnly: true,
            }}
          />
        </Grid>
        <Grid item xs={3}>
          <TextField
            id="outlined-basic"
            defaultValue={email}
            label={t('associateData.email')}
            variant="outlined"
            sx={{ width: 'auto', mb: '10px' }}
            InputProps={{
              readOnly: true,
            }}
          />
        </Grid>
        <Grid item xs={3}>
          <TextField
            id="outlined-basic"
            label={t('associateData.phoneNumber')}
            variant="outlined"
            defaultValue={profile.tlm}
            onChange={(e) => setTLM(e.target.value)}
            sx={{ width: 'auto', mb: '10px' }}
            InputProps={{
              endAdornment: (
                <InputAdornment position="end">
                  <EditIcon></EditIcon>
                </InputAdornment>
              ),
            }}
          />
        </Grid>
        <Grid item xs={3}>
          <TextField
            id="outlined-basic"
            label={t('associateData.nif')}
            defaultValue={profile.nif}
            onChange={(e) => setNIF(e.target.value)}
            variant="outlined"
            sx={{ width: 'auto', mb: '10px' }}
            InputProps={{
              endAdornment: (
                <InputAdornment position="end">
                  <EditIcon></EditIcon>
                </InputAdornment>
              ),
            }}
          />
        </Grid>
        <Grid item xs={3}>
          <TextField
            id="outlined-select-role"
            select
            label="Role"
            value={role}
            sx={{ width: 'auto', mb: '10px' }}
            onChange={(e) => handleClickOpen(e.target.value)}
          >
            {roles.map((option) => (
              <MenuItem key={option.value} value={option.value}>
                {option.label}
              </MenuItem>
            ))}
          </TextField>
        </Grid>
      </Grid>
      <Box
        m={1}
        // margin
        display="flex"
        justifyContent="center"
        alignItems="center"
      >
        <Button
          type="submit"
          size="medium"
          variant="contained"
          style={style}
          onClick={() => setOpen(true)}
        >
          {t('table.quotes.submit')}
        </Button>
      </Box>
      <Typography color="#8984f0" fontSize="1.3rem" mb="20px" mt="30px">
        {t('table.title')}
      </Typography>
      <TableAssociate
        rows={subscriptions}
        handleSubmitInvoice={handleSubmitInvoice}
        style={style}
        styleSimple={styleSimple}
      ></TableAssociate>
      <Box
        m={1}
        // margin
        display="flex"
        justifyContent="right"
        alignItems="right"
      ></Box>
      <Dialog
        open={openRole}
        onClose={handleClose}
        aria-labelledby="alert-dialog-title"
        aria-describedby="alert-dialog-description"
        sx={{ height: '100vh' }}
      >
        <DialogTitle id="alert-dialog-title" sx={{ mt: '50px' }}>
          {t('associateData.alert.role')}
        </DialogTitle>
        <DialogActions sx={{ mb: '50px' }}>
          <Button
            onClick={handleClose}
            variant="contained"
            to={'/sign-in'}
            size="medium "
            style={styleSimple}
          >
            {t('associatesTable.alert.no')}
          </Button>
          <Button
            onClick={() => handleRoleAfirmation()}
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
      <Dialog
        open={open}
        onClose={() => setOpen(false)}
        aria-labelledby="alert-dialog-title"
        aria-describedby="alert-dialog-description"
        sx={{ height: '100vh' }}
      >
        <DialogTitle id="alert-dialog-title" sx={{ mt: '50px' }}>
          {t('associateData.alert.profile') + ' ' + email + ' ?'}
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
            onClick={() => handleEditAfirmation()}
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
    </Box>
  )
}

export default DataAssociate
