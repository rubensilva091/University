import { takeLatest, call, put, fork, delay, select } from 'redux-saga/effects'
import actions, { quotasTypes } from '../redux'

function* fetchAssociateSubscriptionHistory({ api }, { payload }) {
  const { response, error, status } = yield call(
    api.fetchAssociateSubscriptionHistory,
    payload,
  )
  if (response) {
    yield put(actions.fetchQuotasSuccess(response.data))
  } else {
    yield put(actions.fetchQuotasFailed(status))
    yield delay(1000)
  }
}

// WATCHER
function* watchFetchAssociateSubscriptionHistory(services) {
  yield takeLatest(
    quotasTypes.FETCH_QUOTAS_REQUEST,
    fetchAssociateSubscriptionHistory,
    services,
  )
}

export default function* root(services) {
  yield fork(watchFetchAssociateSubscriptionHistory, services)
}
