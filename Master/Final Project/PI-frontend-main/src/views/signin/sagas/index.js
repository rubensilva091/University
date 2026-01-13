import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { signinTypes } from '../redux'

function* loginMagicLink({ api }, { payload }) {
  const { response, error, status } = yield call(api.loginMagicLink, payload)
  if (response) {
    yield put(actions.newLoginMagicLinkSuccess())
  } else {
    yield put(actions.newLoginMagicLinkFailed(status))
    yield delay(1000)
  }
}

function* loginPassword({ api }, { payload }) {
  const { response, error, status } = yield call(api.loginPassword, payload)
  if (response) {
    yield put(actions.newLoginPasswordSuccess(response))
  } else {
    yield put(actions.newLoginPasswordFailed(status))
    yield delay(1000)
  }
}

// WATCHER
function* watchSetLoginMagicLink(services) {
  yield takeLatest(
    signinTypes.NEW_LOGIN_MAGIC_LINK_REQUEST,
    loginMagicLink,
    services,
  )
}

function* watchSetLoginPassword(services) {
  yield takeLatest(
    signinTypes.NEW_LOGIN_PASSWORD_REQUEST,
    loginPassword,
    services,
  )
}

export default function* root(services) {
  yield fork(watchSetLoginMagicLink, services)
  yield fork(watchSetLoginPassword, services)
}
