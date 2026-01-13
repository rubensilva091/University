import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'GET_ACCOUNT_REQUEST',
  'GET_ACCOUNT_SUCCESS',
  'GET_ACCOUNT_FAILED',
])

const creators = createActions(
  types.GET_ACCOUNT_REQUEST,
  types.GET_ACCOUNT_SUCCESS,
  types.GET_ACCOUNT_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  id: '',
  firstName: '',
  lastName: '',
  email: '',
  nif: '',
  tlm: '',
  role: '',

  subscriptionStatus: '',

  error: null,
  errorStatus: null,
}

export const reducer = handleActions(
  {
    [types.GET_ACCOUNT_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.GET_ACCOUNT_SUCCESS]: (state, action) => {
      const { payload } = action
      const { profile } = payload
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        firstName: profile.firstName,
        lastName: profile.lastName,
        id: profile.ID,
        nif: profile.nif,
        tlm: profile.tlm,
        role: profile.role,
        email: payload.email,
        subscriptionStatus: payload['subscription-status'],
      })
    },
    [types.GET_ACCOUNT_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        error: payload.error,
        errorStatus: payload.status,
      })
    },
  },
  initialState,
)

export const dashboardTypes = types

export default creators
