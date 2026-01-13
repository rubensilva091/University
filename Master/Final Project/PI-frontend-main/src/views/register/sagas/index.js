import { takeLatest, call, put, fork, delay } from 'redux-saga/effects'
import actions, { registerTypes } from '../redux'

function* fetchCategories({ api }) {
  const { response, error } = yield call(api.fetchCategories)
  if (response) {
    yield put(actions.fetchCategoriesSuccess(response.data))
  } else {
    yield put(actions.fetchCategoriesFailed())
    yield delay(1000)
  }
}

function* register({ api }, { payload }) {
  const { response, error } = yield call(api.register, payload)
  if (response) {
    yield put(actions.createAccountSuccess(response))
  } else {
    yield put(actions.createAccountFailed(error))
    yield delay(1000)
  }
}

// WATCHERS
function* watchFetchCategories(services) {
  yield takeLatest(
    registerTypes.FETCH_CATEGORIES_REQUEST,
    fetchCategories,
    services,
  )
}

function* watchRegister(services) {
  yield takeLatest(registerTypes.CREATE_ACCOUNT_REQUEST, register, services)
}

export default function* root(services) {
  yield fork(watchFetchCategories, services)
  yield fork(watchRegister, services)
}
