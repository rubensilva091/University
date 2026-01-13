import { combineReducers } from 'redux'

import configStore from './configStore'
import rootSaga from '../sagas/'

import { reducer as signinReducer } from '../views/signin/redux'
import { reducer as globalStateReducer } from './globalStateRedux'
import { reducer as adminDashboardReducer } from '../views/dashboard/admin/redux'
import { reducer as quotasReducer } from '../views/quotas/redux'
import { reducer as registerReducer } from '../views/register/redux'
import { reducer as associatesReducer } from '../views/associates/redux'
import { reducer as paymentReducer } from '../views/payment/redux'
import { reducer as scanReducer } from '../views/scan/redux'
import { reducer as blankReducer } from '../views/blank/redux'
import { reducer as associateHistoricReducer } from '../views/historicAssociate/redux'
import { reducer as paymentSettingsReducer } from '../views/paymentSettings/redux'

const appReducer = combineReducers({
  signin: signinReducer,
  adminDashboard: adminDashboardReducer,
  quotas: quotasReducer,
  register: registerReducer,
  associates: associatesReducer,
  payment: paymentReducer,
  scan: scanReducer,
  blank: blankReducer,
  global: globalStateReducer,
  associateProfile: associateHistoricReducer,
  paymentSettings: paymentSettingsReducer,
})

export default () => {
  const configuredStore = configStore(appReducer, rootSaga)
  const { store, sagaMiddleware } = configuredStore
  let { sagasManager } = configuredStore

  /* eslint-disable global-require */
  if (module.hot) {
    module.hot.accept(() => {
      const nextRootReducer = require('./').appReducer
      store.replaceReducer(nextRootReducer)

      const newYieldedSagas = require('../sagas').default
      sagasManager.cancel()
      sagasManager.done.then(() => {
        sagasManager = sagaMiddleware.run(newYieldedSagas)
      })
    })
  }
  /* eslint-enable global-require */

  return store
}
