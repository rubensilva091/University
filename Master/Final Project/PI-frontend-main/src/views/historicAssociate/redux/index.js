import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'CHANGE_ROLE_REQUEST',
  'CHANGE_ROLE_SUCCESS',
  'CHANGE_ROLE_FAILED',

  'UPDATE_PROFILE_REQUEST',
  'UPDATE_PROFILE_SUCCESS',
  'UPDATE_PROFILE_FAILED',

  'MANUAL_SUBSCRIPTION_REQUEST',
  'MANUAL_SUBSCRIPTION_SUCCESS',
  'MANUAL_SUBSCRIPTION_FAILED',

  'UPLOAD_INVOICE_REQUEST',
  'UPLOAD_INVOICE_SUCCESS',
  'UPLOAD_INVOICE_FAILED',
])

const creators = createActions(
  types.CHANGE_ROLE_REQUEST,
  types.CHANGE_ROLE_SUCCESS,
  types.CHANGE_ROLE_FAILED,

  types.UPDATE_PROFILE_REQUEST,
  types.UPDATE_PROFILE_SUCCESS,
  types.UPDATE_PROFILE_FAILED,

  types.MANUAL_SUBSCRIPTION_REQUEST,
  types.MANUAL_SUBSCRIPTION_SUCCESS,
  types.MANUAL_SUBSCRIPTION_FAILED,

  types.UPLOAD_INVOICE_REQUEST,
  types.UPLOAD_INVOICE_SUCCESS,
  types.UPLOAD_INVOICE_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  newRole: null,
  newSub: null,
  errorStatus: null,
}

export const reducer = handleActions(
  {
    [types.CHANGE_ROLE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.CHANGE_ROLE_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        newRole: payload.newRole,
      })
    },
    [types.CHANGE_ROLE_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.UPDATE_PROFILE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.UPDATE_PROFILE_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
      })
    },
    [types.UPDATE_PROFILE_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.MANUAL_SUBSCRIPTION_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.MANUAL_SUBSCRIPTION_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        newSub: true,
      })
    },
    [types.MANUAL_SUBSCRIPTION_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.UPLOAD_INVOICE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.UPLOAD_INVOICE_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        newInvoice: payload.invoice,
      })
    },
    [types.UPLOAD_INVOICE_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
  },
  initialState,
)

export const associateProfileTypes = types

export default creators
