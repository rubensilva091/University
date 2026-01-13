import mirrorCreator from 'mirror-creator'
import { createActions, handleActions } from 'redux-actions'

const types = mirrorCreator([
  'FETCH_SUMMARY_REQUEST',
  'FETCH_SUMMARY_SUCCESS',
  'FETCH_SUMMARY_FAILED',
])

const creators = createActions(
  types.FETCH_SUMMARY_REQUEST,
  types.FETCH_SUMMARY_SUCCESS,
  types.FETCH_SUMMARY_FAILED,
)

export const initialState = {
  loading: false,
  success: false,
  failure: false,

  associatesHistory: [],
  activeAssociates: null,
  inactiveAssociates: null,
  monthlyIncome: [],
  havePaid: null,
  neverPaid: null,
  totalAssociates: null,
  status: null,
}

export const reducer = handleActions(
  {
    [types.FETCH_SUMMARY_REQUEST]: (state) =>
      Object.freeze({
        ...state,
        loading: true,
        success: false,
        failure: false,
      }),
    [types.FETCH_SUMMARY_SUCCESS]: (state, action) => {
      const { payload } = action
      return Object.freeze({
        ...state,
        loading: false,
        success: true,
        failure: false,

        associatesHistory: payload.associatesHistory,
        activeAssociates: payload.subscriptionsSituation.activeAssociates,
        inactiveAssociates: payload.subscriptionsSituation.inactiveAssociates,
        monthlyIncome: payload.monthlyIncome,
        havePaid: payload.historicPayers.havePaid,
        neverPaid: payload.historicPayers.neverPaid,
        totalAssociates: payload.totalAssociates,
      })
    },
    [types.FETCH_SUMMARY_FAILED]: (state, action) => {
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

export const adminDashboardTypes = types

export default creators
