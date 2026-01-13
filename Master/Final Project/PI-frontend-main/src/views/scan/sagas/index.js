import { takeLatest, call, put, fork, delay, select } from 'redux-saga/effects'
import actions, { dashboardTypes } from '../redux'

function* getAccount({ api }, { payload }) {
  const { response, error, status } = yield call(api.getAccount, payload)
  if (response) {
    yield put(actions.getAccountSuccess(response.data))
  } else {
    yield put(actions.getAccountFailed({ error, status }))
    yield delay(1000)
  }
}

// WATCHER
function* watchGetAccount(services) {
  yield takeLatest(dashboardTypes.GET_ACCOUNT_REQUEST, getAccount, services)
}

export default function* root(services) {
  yield fork(watchGetAccount, services)
}
