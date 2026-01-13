import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'FETCH_ALL_PRICES_REQUEST',
  'FETCH_ALL_PRICES_SUCCESS',
  'FETCH_ALL_PRICES_FAILED',

  'ADD_PRICE_REQUEST',
  'ADD_PRICE_SUCCESS',
  'ADD_PRICE_FAILED',

  'DELETE_PRICE_REQUEST',
  'DELETE_PRICE_SUCCESS',
  'DELETE_PRICE_FAILED',
])

const creators = createActions(
  types.FETCH_ALL_PRICES_REQUEST,
  types.FETCH_ALL_PRICES_SUCCESS,
  types.FETCH_ALL_PRICES_FAILED,

  types.ADD_PRICE_REQUEST,
  types.ADD_PRICE_SUCCESS,
  types.ADD_PRICE_FAILED,

  types.DELETE_PRICE_REQUEST,
  types.DELETE_PRICE_SUCCESS,
  types.DELETE_PRICE_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  prices: [],
  addPriceSuccess: false,
  addPriceFailed: false,

  deletePriceSuccess: false,
  deletePriceFailed: false,
  error: null,
  errorStatus: null,
}

export const reducer = handleActions(
  {
    [types.FETCH_ALL_PRICES_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_ALL_PRICES_SUCCESS]: (state, action) => {
      const { payload } = action

      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        prices: payload.prices,
      })
    },
    [types.FETCH_ALL_PRICES_FAILED]: (state, action) => {
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

    [types.ADD_PRICE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        addPriceSuccess: false,
        addPriceFailed: false,
      }),
    [types.ADD_PRICE_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        addPriceSuccess: true,
        addPriceFailed: false,
      })
    },
    [types.ADD_PRICE_FAILED]: (state, action) => {
      const { payload } = action

      return Object.freeze({
        ...state,
        loading: false,
        addPriceSuccess: false,
        addPriceFailed: true,

        error: payload.error,
        errorStatus: payload.status,
      })
    },

    [types.DELETE_PRICE_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        deletePriceSuccess: false,
        deletePriceFailed: false,
      }),
    [types.DELETE_PRICE_SUCCESS]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        deletePriceSuccess: true,
        deletePriceFailed: false,
      })
    },
    [types.DELETE_PRICE_FAILED]: (state, action) => {
      const { payload } = action

      return Object.freeze({
        ...state,
        loading: false,
        deletePriceSuccess: false,
        deletePriceFailed: true,

        error: payload.error,
        errorStatus: payload.status,
      })
    },
  },
  initialState,
)

export const paymentSettingsTypes = types

export default creators
