import Box from '@mui/material/Box'
import Card from '@mui/material/Card'
import CardContent from '@mui/material/CardContent'
import Typography from '@mui/material/Typography'
import * as React from 'react'
import { useEffect, useState } from 'react'
import mbWay from '../../icon/mbWay.svg'

const MINUTE = 60

export default function CardMbWay({ amount, phone, orderID }) {
  const [time, setTime] = useState(MINUTE * 5)
  useEffect(() => {
    const interval = setInterval(() => {
      setTime((prevTime) => prevTime - 1)
    }, 1000)

    return () => clearInterval(interval)
  }, [])
  return (
    <Card sx={{ width: 400, mt: '30px' }}>
      <CardContent>
        <Typography sx={{ fontSize: 16, fontWeight: 'bold', pb: 2 }}>
          Detalhes
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Order ID: {orderID || ''}
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Valor: {amount || ''}€
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Telemóvel: {phone || ''}
        </Typography>
        <Typography sx={{ fontSize: '0.875rem', pb: 1 }}>
          Countdown: {Math.floor(time / MINUTE)}:
          {time - Math.floor(time / MINUTE) * MINUTE}
        </Typography>
        <Typography>
          Receberá uma notificação na aplicação MB Way para que confirme o
          pagamento utilizando o seu PIN MBWAY. Após completar esta confirmação,
          o pagamento está concluído e vai aparecer em quotas.
        </Typography>
        <Box sx={{ mt: '20px' }}>
          <img src={mbWay} alt="image-mbway" width={50} />
        </Box>
      </CardContent>
    </Card>
  )
}
