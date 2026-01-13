import { takeLatest, call, put, fork, delay, select } from 'redux-saga/effects'
import actions, { associateProfileTypes } from '../redux'

function* changeRole({ api }, { payload }) {
  const { response, error, status } = yield call(api.changeRole, payload)
  if (response) {
    yield put(actions.changeRoleSuccess(response.data))
  } else {
    yield put(actions.changeRoleFailed({ error, status }))
    yield delay(1000)
  }
}

function* updateProfile({ api }, { payload }) {
  const { response, error, status } = yield call(api.updateProfile, payload)
  if (response) {
    yield put(actions.updateProfileSuccess(response))
  } else {
    yield put(actions.updateProfileFailed({ error, status }))
    yield delay(1000)
  }
}

function* manualSubscriptionPayment({ api }, { payload }) {
  const { response, error, status } = yield call(
    api.manualSubscriptionPayment,
    payload,
  )
  if (response) {
    yield put(actions.manualSubscriptionSuccess(response))
  } else {
    yield put(actions.manualSubscriptionFailed({ error, status }))
    yield delay(1000)
  }
}

function* uploadInvoice({ api }, { payload }) {
  const { response, error, status } = yield call(api.uploadInvoice, payload)
  if (response) {
    yield put(actions.uploadInvoiceSuccess(response))
  } else {
    yield put(actions.uploadInvoiceFailed({ error, status }))
    yield delay(1000)
  }
}

// WATCHERS
function* watchChangeRole(services) {
  yield takeLatest(
    associateProfileTypes.CHANGE_ROLE_REQUEST,
    changeRole,
    services,
  )
}
function* watchUpdateProfile(services) {
  yield takeLatest(
    associateProfileTypes.UPDATE_PROFILE_REQUEST,
    updateProfile,
    services,
  )
}
function* watchManualSubcription(services) {
  yield takeLatest(
    associateProfileTypes.MANUAL_SUBSCRIPTION_REQUEST,
    manualSubscriptionPayment,
    services,
  )
}
function* watchUploadInvoice(services) {
  yield takeLatest(
    associateProfileTypes.UPLOAD_INVOICE_REQUEST,
    uploadInvoice,
    services,
  )
}

export default function* root(services) {
  yield fork(watchChangeRole, services)
  yield fork(watchUpdateProfile, services)
  yield fork(watchManualSubcription, services)
  yield fork(watchUploadInvoice, services)
}
