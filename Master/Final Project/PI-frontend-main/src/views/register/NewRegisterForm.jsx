import { Box, IconButton, InputAdornment, capitalize } from '@mui/material'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useNavigate } from 'react-router-dom'
import actions from './redux'
import { CustomTextField } from '../../components/CustomInput/CustomTextField'
import { CustomAutocomplete } from '../../components/CustomInput/CustomAutocomplete'
import { NumberTextField } from '../../components/CustomInput/NumberTextField'
import { PhoneNumberTextField } from '../../components/CustomInput/PhoneNumberTextField'
import { useDispatch, useSelector } from 'react-redux'
import Toast from '../../components/Toast/index'
import { CustomStepper } from '../../components/Stepper/index'
import { Visibility, VisibilityOff } from '@mui/icons-material'
import { errorReport, testField } from '../../utils/validation'

export const NewRegisterForm = ({ ButtonSubmit, ButtonBack, ButtonNext }) => {
  const navigate = useNavigate()

  const registerData = useSelector((state) => state.register)
  const loading = useSelector((state) => state.register.loading)
  const failure = useSelector((state) => state.register.failure)
  const error = useSelector((state) => state.register.error)
  const step = useSelector((state) => state.register.step)
  const categories = useSelector((state) => state.register.availableCategories)
  const account = useSelector((state) => state.register.account)
  const firstName = useSelector((state) => state.register.firstName)
  const lastName = useSelector((state) => state.register.lastName)
  const nif = useSelector((state) => state.register.nif)
  const phoneNumber = useSelector((state) => state.register.phoneNumber)
  const email = useSelector((state) => state.register.email)
  const category = useSelector((state) => state.register.category)
  const password = useSelector((state) => state.register.password)
  const confirmPassword = useSelector((state) => state.register.confirmPassword)
  const dispatch = useDispatch()

  const [showPassword, setShowPassword] = useState(false)

  const { t } = useTranslation()

  useEffect(() => {
    dispatch(actions.fetchCategoriesRequest({}))
  }, [])

  const testListField = (listFields) => {
    let newErrors = { ...error }
    for (let index = 0; index < listFields.length; index++) {
      const fieldName = listFields[index]
      if (newErrors[fieldName]) return false
    }
    let validFields = true
    listFields.forEach((value) => {
      const errorMessage = testField(value, registerData[value])
      newErrors[value] = errorMessage
      if (errorMessage) validFields = false
    })
    dispatch(actions.updateRegisterError(newErrors))
    return validFields
  }

  const handleSubmit = (e) => {
    e.preventDefault()

    if (testListField(form[step].test)) {
      if (step !== form.length - 1) {
        dispatch(actions.nextRegisterStep())
      } else if (
        testListField([
          'firstName',
          'lastName',
          'email',
          'nif',
          'phoneNumber',
          'category',
          'password',
          'confirmPassword',
        ])
      ) {
        const payload = {
          email,
          nif,
          category,
          firstName: firstName?.toLowerCase(),
          lastName: lastName?.toLowerCase(),
          tlm: phoneNumber.slice(6),
          password,
        }
        dispatch(actions.createAccountRequest(payload))
      }
    }
  }

  function getDescriptionByCategory(name) {
    const category = categories.find((category) => category.name === name)
    return category ? category.description : null
  }

  // if (error?.email === 'email-uniqueness') {
  //   const url = '/blank?old=true&account=' + email
  //   return <Navigate to={url} />
  // }

  const firstNameField = () => {
    return (
      <CustomTextField
        id="firstName"
        required
        label={t('register.firstName')}
        autoComplete="given-name"
        value={firstName}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({ field: 'firstName', value: value }),
          )
        }}
        error={error.firstName !== ''}
        helperText={t(errorReport[error.firstName])}
      />
    )
  }

  const lastNameField = () => {
    return (
      <CustomTextField
        id="lastName"
        required
        label={t('register.lastName')}
        autoComplete="additional-name"
        value={lastName}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({ field: 'lastName', value: value }),
          )
        }}
        error={error.lastName !== ''}
        helperText={t(errorReport[error.lastName])}
      />
    )
  }

  const emailField = () => {
    return (
      <CustomTextField
        id="email"
        required
        label={t('register.email')}
        value={email}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({
              field: 'email',
              value: value.toLocaleLowerCase().split(' ').join(''),
            }),
          )
        }}
        error={error.email !== ''}
        helperText={t(errorReport[error.email])}
      />
    )
  }

  const phoneNumberField = () => {
    return (
      <PhoneNumberTextField
        id="phoneNumber"
        label={t('register.phoneNumber')}
        value={phoneNumber}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({ field: 'phoneNumber', value: value }),
          )
        }}
        error={error.phoneNumber !== ''}
        helperText={t(errorReport[error.phoneNumber])}
      />
    )
  }

  const nifField = () => {
    return (
      <NumberTextField
        id="nif"
        required
        label={t('register.nif')}
        value={nif}
        onValueChange={(value) => {
          dispatch(actions.updateRegisterField({ field: 'nif', value: value }))
        }}
        error={error.nif !== ''}
        helperText={t(errorReport[error.nif])}
        decimalScale={0}
      />
    )
  }

  const categoryField = () => {
    return (
      <CustomAutocomplete
        id="category"
        required
        disableClearable
        options={categories.map((item) => item.name)}
        label={t('register.category')}
        value={category}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({ field: 'category', value: value }),
          )
        }}
        inputProps={{
          sx: { textTransform: 'capitalize' },
        }}
        renderOption={(props, category) => (
          <Box component="li" {...props}>
            {capitalize(category)} - {getDescriptionByCategory(category)}
          </Box>
        )}
        error={error.category !== ''}
        helperText={t(errorReport[error.category])}
      />
    )
  }

  const passwordField = () => {
    return (
      <>
        <input
          type="text"
          autoComplete="username"
          style={{ display: 'none' }}
        />
        <CustomTextField
          id="password"
          required
          autoComplete="new-password"
          type={showPassword ? 'text' : 'password'}
          label={t('register.password')}
          value={password}
          onValueChange={(value) => {
            dispatch(
              actions.updateRegisterField({ field: 'password', value: value }),
            )
          }}
          error={error.password !== ''}
          helperText={t(errorReport[error.password])}
          InputProps={{
            endAdornment: (
              <InputAdornment position="end">
                <IconButton
                  aria-label="toggle password visibility"
                  edge="end"
                  tabIndex={-1}
                  onClick={() => setShowPassword(!showPassword)}
                >
                  {showPassword ? <VisibilityOff /> : <Visibility />}
                </IconButton>
              </InputAdornment>
            ),
          }}
        />
      </>
    )
  }

  const confirmPasswordField = () => {
    return (
      <CustomTextField
        id="confirmPassword"
        required
        autoComplete="new-password"
        type={showPassword ? 'text' : 'password'}
        label={t('register.confirmPassword')}
        value={confirmPassword}
        onValueChange={(value) => {
          dispatch(
            actions.updateRegisterField({
              field: 'confirmPassword',
              value: value,
            }),
          )
        }}
        onBlur={() => {
          if (password !== confirmPassword) {
            dispatch(
              actions.updateRegisterError({
                ...error,
                confirmPassword: 'match',
              }),
            )
          }
        }}
        error={error.confirmPassword !== ''}
        helperText={t(errorReport[error.confirmPassword])}
        InputProps={{
          endAdornment: (
            <InputAdornment position="end">
              <IconButton
                aria-label="toggle password visibility"
                edge="end"
                tabIndex={-1}
                onClick={() => setShowPassword(!showPassword)}
              >
                {showPassword ? <VisibilityOff /> : <Visibility />}
              </IconButton>
            </InputAdornment>
          ),
        }}
      />
    )
  }

  const form = [
    {
      test: ['firstName', 'lastName'],
      form: (
        <>
          {firstNameField()}
          {lastNameField()}
        </>
      ),
    },
    {
      test: ['email', 'phoneNumber'],
      form: (
        <>
          {emailField()}
          {phoneNumberField()}
        </>
      ),
    },
    {
      test: ['nif', 'category'],
      form: (
        <>
          {nifField()}
          {categoryField()}
        </>
      ),
    },
    {
      test: ['password', 'confirmPassword'],
      form: (
        <>
          {passwordField()}
          {confirmPasswordField()}
        </>
      ),
    },
  ]

  useEffect(() => {
    if (failure) {
      for (let index = 0; index < step; index++) {
        if (
          form[index].test.some((item) => {
            if (error[item]) {
              dispatch(actions.updateRegisterStep(index))
              return true
            }
          })
        )
          break
      }
    }
  }, [failure])

  useEffect(() => {
    const element = document.getElementById(form[step].test[0])
    if (element) element.focus()
  }, [step])

  useEffect(() => {
    if (account) navigate('/blank?account=' + account.email)
  }, [account])

  return (
    <Box
      component="form"
      onSubmit={handleSubmit}
      sx={{
        display: 'flex',
        flexDirection: 'column',
        gap: 2,
      }}
    >
      <Box
        sx={{
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'space-between',
          minHeight: '105px',
          gap: 4,
        }}
      >
        {form[step].form}
      </Box>
      <CustomStepper numberOfDots={form.length} current={step} />
      <Box
        sx={{
          display: 'flex',
          flexDirection: { xs: 'column-reverse', sm: 'row' },
          gap: 4,
        }}
      >
        {step === 0 ? null : (
          <ButtonBack onClick={() => dispatch(actions.backRegisterStep())} />
        )}
        {step === form.length - 1 ? (
          <ButtonSubmit loading={loading} type="submit" />
        ) : (
          <ButtonNext type="submit" />
        )}
      </Box>
      {failure ? (
        <Toast status={false} message={t('toasts.registerFalse')}></Toast>
      ) : null}
    </Box>
  )
}
