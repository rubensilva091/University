import React from 'react'
import { useTranslation } from 'react-i18next'
import { CustomTable } from './CustomTable'
import { Link as LinkIcon } from '@mui/icons-material'
import { CustomButton } from '../CustomButton/index'

export const NewPaymentHistoryTable = ({ data, pagination }) => {
  const { t } = useTranslation()

  const formatDate = (date) => {
    if (date) {
      const slice = date.split('T')
      if (slice.length > 0) return slice[0]
    }
    return date
  }

  const columns = [
    {
      id: 'id',
      label: 'Id',
    },
    {
      id: 'price',
      label: t('table.quotes.value'),
      format: (price) => `${price.toFixed(2)}€`,
    },
    {
      id: 'start_date',
      label: t('table.quotes.date'),
      format: (date) => formatDate(date),
    },
    {
      id: 'end_date',
      label: t('table.quotes.finalDate'),
      format: (date) => formatDate(date),
    },
    {
      id: 'invoice',
      label: t('table.quotes.invoice'),
      format: (invoice) => {
        if (invoice !== null)
          return (
            <CustomButton
              styleVariant="toggle"
              aria-label="invoice"
              to={invoice}
              target="_blank"
              endIcon={<LinkIcon />}
              typography={{
                fontSize: '0.875rem',
              }}
            >
              Link to invoice
            </CustomButton>
          )
        else return t('table.quotes.noInvoice')
      },
    },
  ]

  const rows = data.map((row) => ({
    id: row.id,
    price: row.price,
    start_date: row.start_date,
    end_date: row.end_date,
    invoice: row.invoice_file,
  }))

  return <CustomTable columns={columns} rows={rows} pagination={pagination} />
}
