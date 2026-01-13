import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'SET_FIELD',
  'RESET',

  'FETCH_PRICES_REQUEST',
  'FETCH_PRICES_SUCCESS',
  'FETCH_PRICES_FAILED',

  'NEW_MBWAYPAY_REQUEST',
  'NEW_MBWAYPAY_SUCCESS',
  'NEW_MBWAYPAY_FAILED',

  'NEW_MBPAY_REQUEST',
  'NEW_MBPAY_SUCCESS',
  'NEW_MBPAY_FAILED',
])

const creators = createActions(
  types.SET_FIELD,
  types.RESET,

  types.FETCH_PRICES_REQUEST,
  types.FETCH_PRICES_SUCCESS,
  types.FETCH_PRICES_FAILED,

  types.NEW_MBWAYPAY_REQUEST,
  types.NEW_MBWAYPAY_SUCCESS,
  types.NEW_MBWAYPAY_FAILED,

  types.NEW_MBPAY_REQUEST,
  types.NEW_MBPAY_SUCCESS,
  types.NEW_MBPAY_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  prices: [],

  mbPayment: undefined,
  mbWayPayment: undefined,
  mbWayStatus: false,

  error: {
    option: '',
    tlm: '',
    mbWay: '',
    mb: '',
  },

  option: undefined,
  tlm: '',
  jwt: undefined,
  paymentMethod: 'mbway',
  time: undefined,
  expireTime: undefined,
}

export const reducer = handleActions(
  {
    [types.SET_FIELD]: (state, action) =>
      Object.freeze({
        ...state,
        ...action.payload,
      }),
    [types.RESET]: (state) =>
      Object.freeze({
        ...state,
        time: undefined,
        expireTime: undefined,
        mbWayPayment: undefined,
      }),

    [types.FETCH_PRICES_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_PRICES_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        option: payload.prices[0],
        prices: payload.prices,
      })
    },
    [types.FETCH_PRICES_FAILED]: (state, action) =>
      Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
      }),

    [types.NEW_MBWAYPAY_REQUEST]: (state, action) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
        time: new Date().getTime(),
        expireTime: action.payload.expireTime,
      }),
    [types.NEW_MBWAYPAY_SUCCESS]: (state, action) =>
      Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        mbWayPayment: action.payload,
        error: {
          ...state.error,
          mbWay: '',
        },
      }),
    [types.NEW_MBWAYPAY_FAILED]: (state) =>
      Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        error: {
          ...state.error,
          tlm: 'payment.phoneNumberError',
          mbWay: 'Error MBway',
        },
        time: undefined,
        expireTime: undefined,
        mbWayPayment: undefined,
      }),

    [types.NEW_MBPAY_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.NEW_MBPAY_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,
        mbPayment: payload,
        error: {
          ...state.error,
          mb: '',
        },
      })
    },
    [types.NEW_MBPAY_FAILED]: (state, action) =>
      Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        error: {
          ...state.error,
          mb: action.payload.status,
        },
      }),
  },
  initialState,
)

export const payTypes = types

export default creators
