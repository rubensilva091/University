import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'RESEND_CONFIRMATION_REQUEST',
  'RESEND_CONFIRMATION_SUCCESS',
  'RESEND_CONFIRMATION_FAILED',
])

const creators = createActions(
  types.RESEND_CONFIRMATION_REQUEST,
  types.RESEND_CONFIRMATION_SUCCESS,
  types.RESEND_CONFIRMATION_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,
}

export const reducer = handleActions(
  {
    [types.RESEND_CONFIRMATION_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.RESEND_CONFIRMATION_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
      })
    },
    [types.RESEND_CONFIRMATION_FAILED]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
      })
    },
  },
  initialState,
)

export const blankTypes = types

export default creators
