import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'SET_FIELD',

  'FETCH_QUOTAS_REQUEST',
  'FETCH_QUOTAS_SUCCESS',
  'FETCH_QUOTAS_FAILED',
])

const creators = createActions(
  types.SET_FIELD,

  types.FETCH_QUOTAS_REQUEST,
  types.FETCH_QUOTAS_SUCCESS,
  types.FETCH_QUOTAS_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  subscriptionHistory: [],
  endOfList: false,
  error: null,
  jwt: '',
  nextPage: 0,
  pageSize: 8,
}

export const reducer = handleActions(
  {
    [types.SET_FIELD]: (state, action) =>
      Object.freeze({
        ...state,
        ...action.payload,
      }),

    [types.FETCH_QUOTAS_REQUEST]: (state, action) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_QUOTAS_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        nextPage: state.nextPage + 1,
        endOfList: payload['subscription-history'].length < state.pageSize,
        subscriptionHistory: [
          ...state.subscriptionHistory,
          ...payload['subscription-history'],
        ],
      })
    },
    [types.FETCH_QUOTAS_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        error: payload,
      })
    },
  },
  initialState,
)

export const quotasTypes = types

export default creators
