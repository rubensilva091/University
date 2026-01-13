import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { blankTypes } from '../redux'

function* blank({ api }, { payload }) {
  const { response, error } = yield call(api.resendConfirmation, payload)
  if (response) {
    yield put(actions.resendConfirmationSuccess(response))
  } else {
    yield put(actions.resendConfirmationFailed(error))
    yield delay(1000)
  }
}

// WATCHERS
function* watchResendConfirmation(services) {
  yield takeLatest(blankTypes.RESEND_CONFIRMATION_REQUEST, blank, services)
}

export default function* root(services) {
  yield fork(watchResendConfirmation, services)
}
