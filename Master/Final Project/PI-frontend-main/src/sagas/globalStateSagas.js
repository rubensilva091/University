import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { globalTypes } from '../redux/globalStateRedux'

function* fetchAccount({ api }, { payload }) {
  const { response, status } = yield call(api.fetchAccount, payload)
  if (response) {
    yield put(actions.fetchAccountSuccess(response.data))
  } else {
    yield put(actions.fetchAccountFailed(status))
    yield delay(1000)
  }
}

// WATCHER
function* watchFetchAccount(services) {
  yield takeLatest(globalTypes.FETCH_ACCOUNT_REQUEST, fetchAccount, services)
}

export default function* root(services) {
  yield fork(watchFetchAccount, services)
}
