import { createStore, applyMiddleware } from 'redux'
import createSagaMiddleware from 'redux-saga'
import { composeWithDevTools } from 'redux-devtools-extension'

export default (rootReducer, rootSaga) => {
  const middleware = []

  // Saga
  const sagaMiddleware = createSagaMiddleware()
  middleware.push(sagaMiddleware)
  const store = createStore(
    rootReducer,
    composeWithDevTools(applyMiddleware(...middleware)),
  )

  // run root saga
  const sagasManager = sagaMiddleware.run(rootSaga)

  return {
    store,
    sagasManager,
    sagaMiddleware,
  }
}
