import { call, fork } from 'redux-saga/effects'

import api from '../services'

// Sagas
import associatesSagas from '../views/associates/sagas'
import blankSagas from '../views/blank/sagas'
import globalStateSagas from './globalStateSagas'
import adminDashboardSagas from '../views/dashboard/admin/sagas'
import paymentSagas from '../views/payment/sagas'
import quotasSagas from '../views/quotas/sagas'
import registerSagas from '../views/register/sagas'
import scanSagas from '../views/scan/sagas'
import signinSagas from '../views/signin/sagas'
import associateHistoricSaga from '../views/historicAssociate/sagas'
import paymentSettingsSaga from '../views/paymentSettings/sagas'

export function* watch() {
  yield fork(globalStateSagas, api)
  yield fork(signinSagas, api)
  yield fork(quotasSagas, api)
  yield fork(adminDashboardSagas, api)
  yield fork(registerSagas, api)
  yield fork(associatesSagas, api)
  yield fork(paymentSagas, api)
  yield fork(scanSagas, api)
  yield fork(blankSagas, api)
  yield fork(associateHistoricSaga, api)
  yield fork(paymentSettingsSaga, api)
}

export default function* root() {
  try {
    console.log('ROOT')
    yield call(watch)
  } catch (error) {
    console.log('ERROR SAGA ' + error)
  }
}
