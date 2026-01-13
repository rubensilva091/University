import { takeLatest, call, put, fork, delay, select } from 'redux-saga/effects'
import actions, { associatesTypes } from '../redux'

function* fetchAssociates({ api }, { payload }) {
  const { response, error, status } = yield call(api.fetchAssociates, payload)
  if (response) {
    yield put(actions.fetchAssociatesSuccess(response.data))
  } else {
    yield put(actions.fetchAssociatesFailed({ error, status }))
    yield delay(1000)
  }
}

function* disableAssociate({ api }, { payload }) {
  const { response, error, status } = yield call(api.disable, payload)
  if (response) {
    yield put(actions.disableAssociateSuccess(response.data))
  } else {
    yield put(actions.disableAssociateFailed({ error, status }))
    yield delay(1000)
  }
}

function* fetchAssociateData({ api }, { payload }) {
  const { response, error, status } = yield call(
    api.fetchAssociateData,
    payload,
  )
  if (response) {
    yield put(actions.associateDataSuccess(response.data))
  } else {
    yield put(actions.associateDataFailed({ error, status }))
    yield delay(1000)
  }
}

function* verifyAssociate({ api }, { payload }) {
  const { response, error, status } = yield call(
    api.manualVerification,
    payload,
  )
  if (response) {
    yield put(actions.verifyAssociateSuccess(response.data))
  } else {
    yield put(actions.verifyAssociateFailed({ error, status }))
    yield delay(1000)
  }
}

function* downloadAssociate({ api }, { payload }) {
  const { error } = yield call(api.downloadAssociate, payload)
  if (!error) {
    yield put(actions.downloadAssociateSuccess())
  } else {
    yield put(actions.downloadAssociateFailed(error))
    yield delay(1000)
  }
}

function* downloadAssociates({ api }, { payload }) {
  const { error } = yield call(api.downloadAssociates, payload)
  if (!error) {
    yield put(actions.downloadAssociatesSuccess())
  } else {
    yield put(actions.downloadAssociatesFailed(error))
    yield delay(1000)
  }
}

// WATCHERS
function* watchFetchAssociates(services) {
  yield takeLatest(
    associatesTypes.FETCH_ASSOCIATES_REQUEST,
    fetchAssociates,
    services,
  )
}
function* watchDisableAssociate(services) {
  yield takeLatest(
    associatesTypes.DISABLE_ASSOCIATE_REQUEST,
    disableAssociate,
    services,
  )
}
function* watchAssociateData(services) {
  yield takeLatest(
    associatesTypes.ASSOCIATE_DATA_REQUEST,
    fetchAssociateData,
    services,
  )
}
function* watchVerifyAssociate(services) {
  yield takeLatest(
    associatesTypes.VERIFY_ASSOCIATE_REQUEST,
    verifyAssociate,
    services,
  )
}
function* watchDownloadAssociate(services) {
  yield takeLatest(
    associatesTypes.DOWNLOAD_ASSOCIATE_REQUEST,
    downloadAssociate,
    services,
  )
}
function* watchDownloadAssociates(services) {
  yield takeLatest(
    associatesTypes.DOWNLOAD_ASSOCIATES_REQUEST,
    downloadAssociates,
    services,
  )
}

export default function* root(services) {
  yield fork(watchFetchAssociates, services)
  yield fork(watchDisableAssociate, services)
  yield fork(watchAssociateData, services)
  yield fork(watchVerifyAssociate, services)
  yield fork(watchDownloadAssociate, services)
  yield fork(watchDownloadAssociates, services)
}
