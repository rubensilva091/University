import { useCallback, useEffect, useMemo, useState } from 'react'

export const Roles = {
  Admin: 'admin',
  Associate: 'associate',
}

const useStorage = () => {
  const getItem = (key, type) => {
    return window.localStorage[key]
  }

  const setItem = (key, value, type) => {
    window.localStorage.setItem(key, value)
    return true
  }

  const removeItem = (key, type) => {
    window.localStorage.removeItem(key)
  }

  return {
    getItem,
    setItem,
    removeItem,
  }
}

const localStorageListeners = {}

export function useLocalStorageStringState(key, defaultState = null) {
  const storage = useStorage()

  const state = storage.getItem(key) || defaultState

  const [, notify] = useState(key + '\n' + state)

  useEffect(() => {
    if (!localStorageListeners[key]) {
      localStorageListeners[key] = []
    }
    localStorageListeners[key].push(notify)
    return () => {
      localStorageListeners[key] = localStorageListeners[key].filter(
        (listener) => listener !== notify,
      )
      if (localStorageListeners[key].length === 0) {
        delete localStorageListeners[key]
      }
    }
  }, [key])

  const setState = useCallback(
    (newState) => {
      const changed = state !== newState
      if (!changed) {
        return
      }

      if (newState === null) {
        storage.removeItem(key)
      } else {
        storage.setItem(key, newState)
      }
      localStorageListeners[key]?.forEach((listener) =>
        listener(key + '\n' + newState),
      )
    },
    [state, key],
  )

  return [state, setState]
}

export function useLocalStorageState(key, defaultState = null) {
  const [stringState, setStringState] = useLocalStorageStringState(
    key,
    JSON.stringify(defaultState),
  )

  return [
    useMemo(
      () =>
        stringState &&
        stringState !== 'undefined' &&
        JSON.parse(stringState || ''),
      [stringState],
    ),
    (newState) => setStringState(JSON.stringify(newState)),
  ]
}

export function capitalizeWords(name) {
  if (!name) return name
  const words = name.split(' ')
  const res = []
  words.forEach((word) => {
    if (word.length > 0) {
      res.push(word[0].toUpperCase() + word.substring(1))
    }
  })
  return res.join(' ')
}

export default function formatDate(date) {
  if (date) {
    const slice = date.split('T')
    if (slice.length > 0) return slice[0]
  }
  return date
}

export const validateEmail = (email) => {
  return String(email)
    .toLowerCase()
    .match(
      /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|.(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/,
    )
}
