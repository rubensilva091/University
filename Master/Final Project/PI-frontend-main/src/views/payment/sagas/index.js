import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { payTypes } from '../redux'

function* fetchPrices({ api }, { payload }) {
  const { response, error } = yield call(api.fetchPrices, payload)
  if (response) {
    yield put(actions.fetchPricesSuccess(response.data))
  } else {
    yield put(actions.fetchPricesFailed())
    yield delay(1000)
  }
}

function* mbWayPay({ api }, { payload }) {
  const { response, error } = yield call(api.mbWayPay, payload)
  if (response) {
    yield put(actions.newMbwaypaySuccess(response.data))
  } else {
    yield put(actions.newMbwaypayFailed())
    yield delay(1000)
  }
}

function* mbPay({ api }, { payload }) {
  const { response, error } = yield call(api.mbPay, payload)
  if (response) {
    yield put(actions.newMbpaySuccess(response.data))
  } else {
    yield put(actions.newMbpayFailed())
    yield delay(1000)
  }
}

// WATCHERS
function* watchFetchPrices(services) {
  yield takeLatest(payTypes.FETCH_PRICES_REQUEST, fetchPrices, services)
}

function* watchMbWayPay(services) {
  yield takeLatest(payTypes.NEW_MBWAYPAY_REQUEST, mbWayPay, services)
}

function* watchMbPay(services) {
  yield takeLatest(payTypes.NEW_MBPAY_REQUEST, mbPay, services)
}

export default function* root(services) {
  yield fork(watchFetchPrices, services)
  yield fork(watchMbWayPay, services)
  yield fork(watchMbPay, services)
}
