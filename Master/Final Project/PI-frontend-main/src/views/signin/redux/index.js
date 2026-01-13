import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'NEW_LOGIN_MAGIC_LINK_REQUEST',
  'NEW_LOGIN_MAGIC_LINK_SUCCESS',
  'NEW_LOGIN_MAGIC_LINK_FAILED',
  'NEW_LOGIN_PASSWORD_REQUEST',
  'NEW_LOGIN_PASSWORD_SUCCESS',
  'NEW_LOGIN_PASSWORD_FAILED',
  'UPDATE_LOGIN_EMAIL',
  'UPDATE_LOGIN_PASSWORD',
  'UPDATE_LOGIN_ERROR',
  'RESET_LOGIN',
])

const creators = createActions(
  types.NEW_LOGIN_MAGIC_LINK_REQUEST,
  types.NEW_LOGIN_MAGIC_LINK_SUCCESS,
  types.NEW_LOGIN_MAGIC_LINK_FAILED,
  types.NEW_LOGIN_PASSWORD_REQUEST,
  types.NEW_LOGIN_PASSWORD_SUCCESS,
  types.NEW_LOGIN_PASSWORD_FAILED,
  types.UPDATE_LOGIN_EMAIL,
  types.UPDATE_LOGIN_PASSWORD,
  types.UPDATE_LOGIN_ERROR,
  types.RESET_LOGIN,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  statusCode: undefined,
  response: undefined,

  email: '',
  password: '',
  error: {
    email: '',
    credentials: '',
    global: '',
  },
}

export const reducer = handleActions(
  {
    [types.NEW_LOGIN_MAGIC_LINK_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
        statusCode: undefined,
        error: {
          email: '',
          credentials: '',
          global: '',
        },
      }),
    [types.NEW_LOGIN_MAGIC_LINK_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        password: '',
      })
    },
    [types.NEW_LOGIN_MAGIC_LINK_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        statusCode: payload,
        password: '',
        error: {
          ...state.error,
          email: payload === 404 ? 'toasts.login404' : '',
          global: payload === 404 ? '' : 'toasts.emailFail',
        },
      })
    },

    [types.NEW_LOGIN_PASSWORD_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
        statusCode: undefined,
        response: undefined,
        error: {
          email: '',
          credentials: '',
          global: '',
        },
      }),
    [types.NEW_LOGIN_PASSWORD_SUCCESS]: (state, action) =>
      Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        response: action.payload,
        email: '',
        password: '',
      }),
    [types.NEW_LOGIN_PASSWORD_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        statusCode: payload,
        password: '',
        error: {
          ...state.error,
          credentials: payload === 403 ? 'toasts.login403' : '',
          global: payload === 403 ? '' : 'toasts.emailFail',
        },
      })
    },

    [types.UPDATE_LOGIN_EMAIL]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        email: payload,
        statusCode: undefined,
        error: {
          ...state.error,
          email: '',
          credentials: '',
        },
      })
    },
    [types.UPDATE_LOGIN_PASSWORD]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        password: payload,
        statusCode: undefined,
        error: {
          ...state.error,
          credentials: '',
        },
      })
    },
    [types.UPDATE_LOGIN_ERROR]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        error: {
          ...state.error,
          ...payload,
        },
      })
    },
    [types.RESET_LOGIN]: () =>
      Object.freeze({
        loading: false,
        success: false,
        failure: false,
        statusCode: undefined,
        response: undefined,
        email: '',
        password: '',
        error: {
          email: '',
          credentials: '',
          global: '',
        },
      }),
  },
  initialState,
)

export const signinTypes = types

export default creators
