const API_ROOT = process.env.REACT_APP_API_URL

async function callApi(endpoint, fetchOptions) {
  let response
  try {
    const fullUrl = `${API_ROOT}/${endpoint}`

    const defaultOptions = {
      headers: {
        'Content-type': 'application/json',
        Accept: 'application/json',
      },
    }

    const headers = {
      headers: {
        ...defaultOptions.headers,
        ...fetchOptions.headers,
      },
    }

    if (headers.headers.Accept === 'application/csv') {
      response = await fetch(fullUrl, {
        ...fetchOptions,
        ...headers,
      })
        .then((response) => response.blob())
        .then((blob) => {
          const url = window.URL.createObjectURL(new Blob([blob]))

          const link = document.createElement('a')
          link.href = url
          link.download = 'export.csv'

          document.body.appendChild(link)

          link.click()

          link.parentNode.removeChild(link)
        })
      return { error: false }
    } else {
      response = await fetch(fullUrl, {
        ...fetchOptions,
        ...headers,
      })
      if (response.status === 204 && response.ok) {
        return { error: false, status: response.status }
      }

      if (response.status === 500) {
        return { error: 'INVALID_TOKEN', status: response.status }
      }
      if (response.status === 401) {
        return { error: 'Unauthorized', status: response.status }
      }

      const json = await response.json()
      if (response.ok) {
        return { response: json, error: false, status: response.status }
      }

      if (response.status === 404) {
        return { error: json.data, status: response.status }
      }

      if (response.status === 403) {
        return { error: json.data, status: response.status }
      }

      if (response.status === 429) {
        return { error: 'TOO MANY REQUESTS', status: response.status }
      }

      if (response.status === 422) {
        return { error: json.data, status: response.status }
      }

      const errorDetail = json.errors[0].detail
      return {
        error: errorDetail || 'Something bad happened',
        status: response.status,
      }
    }
  } catch (error) {
    console.log('FAILED call API no catch', response)
    console.log('FAILED call API no catch 2', error)
    if (!response) {
      return { error: `Something bad happened ${error}`, status: null }
    }
    if (response.status === 200 && response.ok) {
      return { error: false, status: response.status }
    }
    return { error: `Something bad happened ${error}`, status: null }
  }
}

async function wait(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms)
  })
}

async function retryApiCall(endpoint, fetchOptions, tries) {
  const resp = await callApi(endpoint, fetchOptions)
  if (!resp.error || resp.status === 401) {
    return resp
  }
  if (tries === 0) return resp
  else {
    await wait(2000)
    return retryApiCall(endpoint, fetchOptions, tries - 1)
  }
}

export const loginMagicLink = (payload) => {
  const endpoint = 'email-login'
  const fetchOptions = {
    method: 'POST',
    body: JSON.stringify({ email: payload }),
  }
  return callApi(endpoint, fetchOptions)
}

export const loginPassword = (payload) => {
  const endpoint = 'password-login'
  const fetchOptions = {
    method: 'POST',
    body: JSON.stringify(payload),
  }
  return callApi(endpoint, fetchOptions)
}

export const resendConfirmation = (payload) => {
  const endpoint = 'accounts/resend-confirmation'
  const fetchOptions = {
    method: 'POST',
    body: JSON.stringify({ email: payload }),
  }
  return callApi(endpoint, fetchOptions)
}

export const fetchAccount = (payload) => {
  let endpoint
  if (payload.page && payload.pageSize) {
    endpoint = `associate/status?page=${payload.page}&pageSize=${payload.pageSize}`
  } else {
    endpoint = 'associate/status'
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload}`,
    },
    method: 'GET',
  }

  return retryApiCall(endpoint, fetchOptions, 3)
}

export const fetchAssociateSubscriptionHistory = (payload) => {
  let endpoint
  if (payload.page && payload.pageSize) {
    endpoint = `associate/subscription/history?page=${payload.page}&pageSize=${payload.pageSize}`
  } else {
    endpoint = 'associate/subscription/history'
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }

  return retryApiCall(endpoint, fetchOptions, 3)
}

export const fetchCategories = () => {
  const endpoint = 'payments/categories'
  let fetchOptions
  fetchOptions = {
    method: 'GET',
  }

  return callApi(endpoint, fetchOptions)
}

export const fetchAssociates = (payload) => {
  let endpoint
  endpoint = 'admin/data/associates?'
  if (payload.status) {
    endpoint += `status=${payload.status}&`
  }
  if (payload.subStatus) {
    endpoint += `subStatus=${payload.subStatus}&`
  }
  if (payload.name) {
    endpoint += `name=${payload.name}&`
  }
  if (payload.page) {
    endpoint += `page=${payload.page}&`
  }
  if (payload.page) {
    endpoint += `pageSize=${payload.pageSize}`
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }

  return retryApiCall(endpoint, fetchOptions, 3)
}

export const fetchPrices = (payload) => {
  const endpoint = 'payments/prices'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload}`,
    },
    method: 'GET',
  }

  return callApi(endpoint, fetchOptions)
}

export const mbWayPay = (payload) => {
  const endpoint = 'payments/mbway'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    body: JSON.stringify({ period: payload.period, tlm: payload.tlm }),
    method: 'POST',
  }
  return callApi(endpoint, fetchOptions)
}

export const mbPay = (payload) => {
  const endpoint = 'payments/multibanco'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    body: JSON.stringify({ period: payload.period }),
    method: 'POST',
  }
  return callApi(endpoint, fetchOptions)
}

export const getAccount = (payload) => {
  const endpoint = `associate/scan?id=${payload.id}`
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }

  return callApi(endpoint, fetchOptions)
}

export const register = (payload) => {
  const endpoint = 'accounts'
  const fetchOptions = {
    method: 'POST',
    body: JSON.stringify({
      email: payload.email,
      nif: payload.nif,
      category: payload.category,
      firstName: payload.firstName,
      lastName: payload.lastName,
      tlm: payload.phoneNumber,
      password: payload.password,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const getMBWayStatus = (payload) => {
  const endpoint = `payments/mbway/status?orderID=${payload.orderID}`
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }

  return callApi(endpoint, fetchOptions)
}

export const fetchAssociatesSummary = (payload) => {
  const endpoint = 'admin/data/associates/summary'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload}`,
    },
    method: 'GET',
  }
  return retryApiCall(endpoint, fetchOptions, 3)
}

export const disable = (payload) => {
  const endpoint = 'admin/data/associates/account/disable'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'PUT',
    body: JSON.stringify({
      email: payload.email,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const fetchAssociateData = (payload) => {
  let endpoint
  endpoint = `admin/data/associates/account?email=${payload.email}&`
  if (payload.page && payload.pageSize) {
    endpoint += `page=${payload.page}&pageSize=${payload.pageSize}`
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }
  return callApi(endpoint, fetchOptions)
}

export const changeRole = (payload) => {
  const endpoint = 'admin/data/associates/account/update/role'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'PUT',
    body: JSON.stringify({
      email: payload.email,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const updateProfile = (payload) => {
  const endpoint = 'admin/data/associates/account/update/profile'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'PUT',
    body: JSON.stringify({
      email: payload.email,
      firstName: payload.firstName,
      lastName: payload.lastName,
      nif: payload.nif,
      tlm: payload.tlm,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const manualSubscriptionPayment = (payload) => {
  const endpoint = 'admin/data/associates/account/payment'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'POST',
    body: JSON.stringify({
      accountId: payload.accountId,
      period: payload.period,
      startDate: payload.startDate,
      price: payload.price,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const fetchAllPrices = (payload) => {
  const endpoint = 'admin/data/prices'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload}`,
    },
    method: 'GET',
  }

  return callApi(endpoint, fetchOptions)
}

export const addPrice = (payload) => {
  const endpoint = 'admin/data/prices'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'POST',
    body: JSON.stringify({
      category: payload.category,
      price: payload.price,
      period: payload.period,
    }),
  }

  return callApi(endpoint, fetchOptions)
}

export const deletePrice = (payload) => {
  const endpoint = 'admin/data/prices'
  let fetchOptions
  fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'DELETE',
    body: JSON.stringify({
      category: payload.category,
      period: payload.period,
    }),
  }

  return callApi(endpoint, fetchOptions)
}

export const manualVerification = (payload) => {
  const endpoint = 'admin/data/associates/account/verify'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'PUT',
    body: JSON.stringify({
      email: payload.email,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const uploadInvoice = (payload) => {
  const endpoint = 'admin/data/associates/account/invoice'
  const fetchOptions = {
    headers: {
      Authorization: `${payload.jwt}`,
    },
    method: 'PUT',
    body: JSON.stringify({
      id: payload.id,
      subscriptionId: payload.subscriptionId,
      invoiceUrl: payload.invoiceUrl,
    }),
  }
  return callApi(endpoint, fetchOptions)
}

export const downloadAssociate = (payload) => {
  let endpoint
  endpoint = `admin/data/associates/account/extract?email=${payload.email}&`
  if (payload.page && payload.pageSize) {
    endpoint += `page=${payload.page}&pageSize=${payload.pageSize}`
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      'Content-type': 'application/csv',
      Accept: 'application/csv',
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }
  return callApi(endpoint, fetchOptions)
}

export const downloadAssociates = (payload) => {
  let endpoint
  endpoint = 'admin/data/associates/extract'
  if (payload.page && payload.pageSize) {
    endpoint += `?page=${payload.page}&pageSize=${payload.pageSize}`
  }
  let fetchOptions
  fetchOptions = {
    headers: {
      'Content-type': 'application/csv',
      Accept: 'application/csv',
      Authorization: `${payload.jwt}`,
    },
    method: 'GET',
  }
  return callApi(endpoint, fetchOptions)
}
