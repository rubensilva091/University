import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'FETCH_CATEGORIES_REQUEST',
  'FETCH_CATEGORIES_SUCCESS',
  'FETCH_CATEGORIES_FAILED',
  'CREATE_ACCOUNT_REQUEST',
  'CREATE_ACCOUNT_SUCCESS',
  'CREATE_ACCOUNT_FAILED',
  'UPDATE_REGISTER_FIELD',
  'UPDATE_REGISTER_ERROR',
  'UPDATE_REGISTER_STEP',
  'NEXT_REGISTER_STEP',
  'BACK_REGISTER_STEP',
])

const creators = createActions(
  types.FETCH_CATEGORIES_REQUEST,
  types.FETCH_CATEGORIES_SUCCESS,
  types.FETCH_CATEGORIES_FAILED,
  types.CREATE_ACCOUNT_REQUEST,
  types.CREATE_ACCOUNT_SUCCESS,
  types.CREATE_ACCOUNT_FAILED,
  types.UPDATE_REGISTER_FIELD,
  types.UPDATE_REGISTER_ERROR,
  types.UPDATE_REGISTER_STEP,
  types.NEXT_REGISTER_STEP,
  types.BACK_REGISTER_STEP,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,
  firstName: '',
  lastName: '',
  nif: '',
  phoneNumber: '',
  email: '',
  category: '',
  password: '',
  confirmPassword: '',
  step: 0,

  availableCategories: [],
  account: null,
  error: {
    firstName: '',
    lastName: '',
    nif: '',
    phoneNumber: '',
    email: '',
    category: '',
    password: '',
    confirmPassword: '',
  },
}

export const reducer = handleActions(
  {
    [types.FETCH_CATEGORIES_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_CATEGORIES_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        availableCategories: payload.availableCategories,
      })
    },
    [types.FETCH_CATEGORIES_FAILED]: (state) => {
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
      })
    },
    [types.CREATE_ACCOUNT_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.CREATE_ACCOUNT_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        account: payload.data.account,
      })
    },
    [types.CREATE_ACCOUNT_FAILED]: (state, action) => {
      const { payload } = action
      let errors = { ...state.error }
      Object.entries(payload).forEach(([key, errorCode]) => {
        let correctkey
        switch (key) {
          case 'firstname':
            correctkey = 'firstName'
            break
          case 'lastname':
            correctkey = 'lastName'
            break
          case 'tlm':
            correctkey = 'phoneNumber'
            break
          default:
            correctkey = key
            break
        }
        errors[correctkey] = errorCode
      })
      return Object.freeze({
        ...state,
        loading: false,
        success: false,
        failure: true,
        error: errors,
      })
    },
    [types.UPDATE_REGISTER_FIELD]: (state, action) => {
      const { payload: change } = action

      let newErrors = { ...state.error }
      if (newErrors[change.field]) {
        newErrors[change.field] = ''
        if (change.field === 'password') {
          newErrors.confirmPassword = ''
        }
      }

      return Object.freeze({
        ...state,
        [change.field]: change.value,
        error: newErrors,
      })
    },
    [types.UPDATE_REGISTER_ERROR]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        error: payload,
      })
    },
    [types.UPDATE_REGISTER_STEP]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        step: payload,
      })
    },
    [types.BACK_REGISTER_STEP]: (state) => {
      return Object.freeze({
        ...state,
        step: state.step - 1,
      })
    },
    [types.NEXT_REGISTER_STEP]: (state) => {
      return Object.freeze({
        ...state,
        step: state.step + 1,
      })
    },
  },
  initialState,
)

export const registerTypes = types

export default creators
