import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'FETCH_ACCOUNT_REQUEST',
  'FETCH_ACCOUNT_SUCCESS',
  'FETCH_ACCOUNT_FAILED',
])

const creators = createActions(
  types.FETCH_ACCOUNT_REQUEST,
  types.FETCH_ACCOUNT_SUCCESS,
  types.FETCH_ACCOUNT_FAILED,
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
  subscription: '',
  subscriptionHistory: [],
  subscriptionStatus: '',
  status: null,
}

export const reducer = handleActions(
  {
    [types.FETCH_ACCOUNT_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_ACCOUNT_SUCCESS]: (state, action) => {
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

        subscription: payload.subscription,
        subscriptionHistory: payload['subscription-history'],
        subscriptionStatus: payload['subscription-status'],
      })
    },
    [types.FETCH_ACCOUNT_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        status: payload,
      })
    },
  },
  initialState,
)

export const globalTypes = types

export default creators
