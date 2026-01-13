import { takeLatest, call, put, fork, delay, select } from 'redux-saga/effects'
import actions, { adminDashboardTypes } from '../redux'

function* fetchSummary({ api }, { payload }) {
  const { response, status } = yield call(api.fetchAssociatesSummary, payload)
  if (response) {
    yield put(actions.fetchSummarySuccess(response.data))
  } else {
    yield put(actions.fetchSummaryFailed(status))
    yield delay(1000)
  }
}

// WATCHER

function* watchFetchSummary(services) {
  yield takeLatest(
    adminDashboardTypes.FETCH_SUMMARY_REQUEST,
    fetchSummary,
    services,
  )
}

export default function* root(services) {
  yield fork(watchFetchSummary, services)
}
