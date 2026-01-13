import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'FETCH_ASSOCIATES_REQUEST',
  'FETCH_ASSOCIATES_SUCCESS',
  'FETCH_ASSOCIATES_FAILED',

  'DISABLE_ASSOCIATE_REQUEST',
  'DISABLE_ASSOCIATE_SUCCESS',
  'DISABLE_ASSOCIATE_FAILED',

  'ASSOCIATE_DATA_REQUEST',
  'ASSOCIATE_DATA_SUCCESS',
  'ASSOCIATE_DATA_FAILED',

  'VERIFY_ASSOCIATE_REQUEST',
  'VERIFY_ASSOCIATE_SUCCESS',
  'VERIFY_ASSOCIATE_FAILED',

  'DOWNLOAD_ASSOCIATE_REQUEST',
  'DOWNLOAD_ASSOCIATE_SUCCESS',
  'DOWNLOAD_ASSOCIATE_FAILED',

  'DOWNLOAD_ASSOCIATES_REQUEST',
  'DOWNLOAD_ASSOCIATES_SUCCESS',
  'DOWNLOAD_ASSOCIATES_FAILED',
])

const creators = createActions(
  types.FETCH_ASSOCIATES_REQUEST,
  types.FETCH_ASSOCIATES_SUCCESS,
  types.FETCH_ASSOCIATES_FAILED,

  types.DISABLE_ASSOCIATE_REQUEST,
  types.DISABLE_ASSOCIATE_SUCCESS,
  types.DISABLE_ASSOCIATE_FAILED,

  types.ASSOCIATE_DATA_REQUEST,
  types.ASSOCIATE_DATA_SUCCESS,
  types.ASSOCIATE_DATA_FAILED,

  types.VERIFY_ASSOCIATE_REQUEST,
  types.VERIFY_ASSOCIATE_SUCCESS,
  types.VERIFY_ASSOCIATE_FAILED,

  types.DOWNLOAD_ASSOCIATE_REQUEST,
  types.DOWNLOAD_ASSOCIATE_SUCCESS,
  types.DOWNLOAD_ASSOCIATE_FAILED,

  types.DOWNLOAD_ASSOCIATES_REQUEST,
  types.DOWNLOAD_ASSOCIATES_SUCCESS,
  types.DOWNLOAD_ASSOCIATES_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  associates: [],
  disabledAccount: null,
  associateData: null,
  verifiedAccount: null,
  errorStatus: null,
}

export const reducer = handleActions(
  {
    [types.FETCH_ASSOCIATES_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_ASSOCIATES_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        associates: payload['filtered-query'],
      })
    },
    [types.FETCH_ASSOCIATES_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.DISABLE_ASSOCIATE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.DISABLE_ASSOCIATE_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        disabledAccount: payload,
      })
    },
    [types.DISABLE_ASSOCIATE_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.ASSOCIATE_DATA_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.ASSOCIATE_DATA_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        associateData: payload.associateData,
      })
    },
    [types.ASSOCIATE_DATA_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.VERIFY_ASSOCIATE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.VERIFY_ASSOCIATE_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        verifiedAccount: payload,
      })
    },
    [types.VERIFY_ASSOCIATE_FAILED]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,

        errorStatus: payload.status,
      })
    },
    [types.DOWNLOAD_ASSOCIATE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.DOWNLOAD_ASSOCIATE_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
      })
    },
    [types.DOWNLOAD_ASSOCIATE_FAILED]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
      })
    },
    [types.DOWNLOAD_ASSOCIATES_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.DOWNLOAD_ASSOCIATES_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
      })
    },
    [types.DOWNLOAD_ASSOCIATES_FAILED]: (state) => {
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

export const associatesTypes = types

export default creators
