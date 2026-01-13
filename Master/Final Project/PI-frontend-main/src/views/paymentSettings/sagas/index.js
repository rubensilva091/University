import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { paymentSettingsTypes } from '../redux'

function* fetchAllPrices({ api }, { payload }) {
  const { response, error, status } = yield call(api.fetchAllPrices, payload)
  if (response) {
    yield put(actions.fetchAllPricesSuccess(response.data))
  } else {
    yield put(actions.fetchAllPricesFailed({ error, status }))
    yield delay(1000)
  }
}

function* addPrice({ api }, { payload }) {
  const { response, error, status } = yield call(api.addPrice, payload)
  if (response) {
    yield put(actions.addPriceSuccess(response.data))
  } else {
    yield put(actions.addPriceFailed({ error, status }))
    yield delay(1000)
  }
}

function* deletePrice({ api }, { payload }) {
  const { response, error, status } = yield call(api.deletePrice, payload)
  if (response) {
    yield put(actions.deletePriceSuccess(response.data))
  } else {
    yield put(actions.deletePriceFailed({ error, status }))
    yield delay(1000)
  }
}

// WATCHERS
function* watchFetchAllPrices(services) {
  yield takeLatest(
    paymentSettingsTypes.FETCH_ALL_PRICES_REQUEST,
    fetchAllPrices,
    services,
  )
}

function* watchAddPrice(services) {
  yield takeLatest(paymentSettingsTypes.ADD_PRICE_REQUEST, addPrice, services)
}

function* watchDeletePrice(services) {
  yield takeLatest(
    paymentSettingsTypes.DELETE_PRICE_REQUEST,
    deletePrice,
    services,
  )
}

export default function* root(services) {
  yield fork(watchFetchAllPrices, services)
  yield fork(watchAddPrice, services)
  yield fork(watchDeletePrice, services)
}
