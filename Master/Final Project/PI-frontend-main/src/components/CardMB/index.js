import * as React from 'react'
import Box from '@mui/material/Box'
import Card from '@mui/material/Card'
import CardContent from '@mui/material/CardContent'
import Typography from '@mui/material/Typography'
import mb from '../../icon/mb.svg'

export default function CardMB({ reference, entity, amount, expiricyDate }) {
  return (
    <Card sx={{ width: 400, mt: '10px' }}>
      <CardContent>
        <Typography sx={{ fontSize: 16, fontWeight: 'bold', pb: 2 }}>
          Dados de pagamento
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Referência: {reference}
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Entidade: {entity}
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Valor: {amount}€
        </Typography>
        <Typography sx={{ fontSize: 16, fontWeight: 'bold' }}>
          Expira em:
        </Typography>
        <Typography variant="body2" mb="8px">
          {expiricyDate}
        </Typography>
        <Typography mb="10px">
          O pagamento da referência do Multibanco pode ser feita no seu
          homebanking ou numa caixa multibanco.
        </Typography>
        <Box>
          <img src={mb} alt="image" width={50} />
        </Box>
      </CardContent>
    </Card>
  )
}
