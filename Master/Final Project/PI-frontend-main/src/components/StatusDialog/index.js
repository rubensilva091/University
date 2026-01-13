import DialogTitle from '@mui/material/DialogTitle'
import Dialog from '@mui/material/Dialog'
import DialogActions from '@mui/material/DialogActions'
import { useTranslation } from 'react-i18next'
import * as React from 'react'
import { Button } from '@mui/material'

export default function StatusDialog({
  handleClose,
  account,
  styleSimple,
  style,
  handleAfirmation,
  disable,
  open,
}) {
  const { t } = useTranslation()

  return (
    <Dialog
      open={open}
      onClose={handleClose}
      aria-labelledby="alert-dialog-title"
      aria-describedby="alert-dialog-description"
      sx={{ height: '80vh' }}
    >
      <DialogTitle id="alert-dialog-title" sx={{ mt: '50px' }}>
        {disable
          ? t('associatesTable.alert.disable') + ' ' + account + ' ?'
          : t('associatesTable.alert.verify') + ' ' + account + ' ?'}
      </DialogTitle>
      <DialogActions sx={{ mb: '50px' }}>
        <Button
          variant="contained"
          size="medium "
          style={styleSimple}
          onClick={() => handleAfirmation(account)}
        >
          {t('associatesTable.alert.yes')}
        </Button>
        <Button
          onClick={handleClose}
          sx={{ mr: '24px' }}
          size="medium "
          style={style}
          variant="contained"
        >
          {t('associatesTable.alert.no')}
        </Button>
      </DialogActions>
    </Dialog>
  )
}
