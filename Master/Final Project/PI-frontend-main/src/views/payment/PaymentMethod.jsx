import React, { useEffect } from 'react'
import { NewPaymentMethodSelector } from '../../components/PaymentMethodSelector/NewPaymentMethodSelector'
import { useSelector, useDispatch } from 'react-redux'
import actions from './redux'
import Pusher from 'pusher-js'
import { NewPayMBWay } from './PayMBWay/NewPayMBWay'
import { NewPayMB } from './PayMB/NewPayMB'

export const PaymentMethod = () => {
  const paymentMethod = useSelector((state) => state.payment.paymentMethod)
  const mbWayPayment = useSelector((state) => state.payment.mbWayPayment)
  const id = useSelector((state) => state.global.id)
  const dispatch = useDispatch()

  useEffect(() => {
    if (mbWayPayment?.orderID) {
      const pusher = new Pusher(process.env.REACT_APP_PUSHER_KEY, {
        cluster: 'eu',
      })
      const channel = pusher.subscribe(id + '-mbway')
      channel.bind('mbway', function (data) {
        dispatch(actions.setField({ mbWayStatus: true }))
      })
    }
  }, [mbWayPayment])

  return (
    <>
      <NewPaymentMethodSelector
        value={paymentMethod}
        onChange={(value) =>
          dispatch(actions.setField({ paymentMethod: value }))
        }
      />
      {paymentMethod === 'mb' ? <NewPayMB /> : <NewPayMBWay />}
    </>
  )
}
