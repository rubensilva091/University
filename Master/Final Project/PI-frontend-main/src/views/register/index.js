import { Button } from '@mui/material'
import AppBar from '@mui/material/AppBar'
import Box from '@mui/material/Box'
import Container from '@mui/material/Container'
import FormControl from '@mui/material/FormControl'
import MenuItem from '@mui/material/MenuItem'
import Select from '@mui/material/Select'
import TextField from '@mui/material/TextField'
import Toolbar from '@mui/material/Toolbar'
import Typography from '@mui/material/Typography'
import React, { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useDispatch, useSelector } from 'react-redux'
import { Link, Navigate } from 'react-router-dom'
import { NumberTextField } from '../../components/NumberTextField'
import Toast from '../../components/Toast'
import logotipo from '../../icon/logotipo.svg'
import actions from './redux'

export const RegisterView = () => {
  const dispatch = useDispatch()
  const loading = useSelector((state) => state.register.loading)
  const success = useSelector((state) => state.register.success)
  const failure = useSelector((state) => state.register.failure)
  const error = useSelector((state) => state.register.error)
  const categories = useSelector((state) => state.register.availableCategories)
  const account = useSelector((state) => state.register.account)

  useEffect(() => {
    dispatch(actions.fetchCategoriesRequest({}))
  }, [])

  const style = {
    background: 'linear-gradient(45deg,#8984f0  30%, #8984f0  90%)',
    borderRadius: 3,
    border: 0,
    color: 'white',
    height: 48,
    marginLeft: '10px',
    // margin: "30px  20px",
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  const styleSimple = {
    background: 'white',
    borderRadius: 3,
    border: 0,
    color: '#8984f0',
    height: 48,
    // margin: "30px  20px",
    padding: '0 30px',
    boxShadow: '0 3px 5px 2px rgba(255, 105, 135, .3)',
  }

  const {
    t,
    i18n: { changeLanguage, language },
  } = useTranslation()
  const handleChangeLanguage = () => {
    const newLanguage = language === 'en' ? 'pt' : 'en'
    changeLanguage(newLanguage)
  }

  const failedMessage = t('toasts.registerFalse')

  const [firstName, setFirstName] = useState('')
  const [lastName, setLastName] = useState('')
  const [nif, setNif] = useState('')
  const [numberPhone, setNumberPhone] = useState('')
  const [email, setEmail] = useState('')
  const [category, setCategory] = useState('')

  const createPost = () => {
    const payload = {
      email,
      nif,
      category,
      firstName: firstName?.toLowerCase(),
      lastName: lastName?.toLowerCase(),
      tlm: numberPhone,
    }
    dispatch(actions.createAccountRequest(payload))
  }

  const validation = {
    required: t('validation.required'),
    len: t('validation.length'),
    email: t('validation.email'),
    'len=9|len=0': t('validation.length'),
    'email-uniqueness': t('validation.emailUnique'),
    'nif-uniqueness': t('validation.nifUnique'),
  }

  const handleValidation = (key, value) => {
    if (validation[value]) {
      error[key] = validation[value]
    }
  }

  if (error) {
    const keys = Object.keys(error)
    keys.forEach((key, index) => {
      handleValidation(key, error[key])
    })
    if (error?.email === t('validation.emailUnique')) {
      const url = '/blank?old=true&account=' + email
      return <Navigate to={url} />
    }
  }

  if (account) {
    const url = '/blank?account=' + email
    return <Navigate to={url} />
  }

  return (
    <>
      <AppBar position="static">
        <Toolbar
          sx={{ justifyContent: 'space-between', backgroundColor: 'white' }}
        >
          <Link to="/">
            <img src={logotipo} alt="image" height="50px" />
          </Link>
          <FormControl size="small">
            {/* <InputLabel id="demo-simple-select-label">
              {t("language.language")}
            </InputLabel> */}
            <Select
              labelId="demo-simple-select-label"
              id="demo-simple-select"
              value={language}
              // label="Language"
              onChange={handleChangeLanguage}
            >
              <MenuItem value={'pt'}>{t('language.portuguese')}</MenuItem>
              <MenuItem value={'en'}>{t('language.english')}</MenuItem>
            </Select>
          </FormControl>
        </Toolbar>
      </AppBar>
      <Container component="main" maxWidth="xs">
        <Box
          component="form"
          sx={{
            marginTop: 3,
            '& .MuiTextField-root': { m: 1, mb: '15px', width: '25ch' },
          }}
          noValidate
          autoComplete="off"
        >
          <Typography
            component="h1"
            variant="h4"
            marginBottom="20px"
            color="#8984f0"
            fontWeight="fontWeightBold"
            // fontWeight="fontWeightMedium"
          >
            {t('register.createAccount')}
          </Typography>
          <TextField
            error={error?.firstName !== ''}
            helperText={error?.firstName}
            value={firstName}
            onChange={(e) => setFirstName(e.target.value)}
            variant="outlined"
            label={t('register.firstName')}
            required
          ></TextField>
          <TextField
            error={error?.lastName !== ''}
            helperText={error?.lastName}
            value={lastName}
            onChange={(e) => setLastName(e.target.value)}
            variant="outlined"
            label={t('register.lastName')}
            required
          ></TextField>

          <NumberTextField
            error={error?.nif !== ''}
            helperText={error?.nif}
            value={nif}
            setNumber={setNif}
            variant="outlined"
            label={t('register.nif')}
            required
            inputProps={{ style: { textTransform: 'lowercase' } }}
          />

          <NumberTextField
            error={error?.phoneNumber !== ''}
            helperText={error?.phoneNumber}
            value={numberPhone}
            setNumber={setNumberPhone}
            variant="outlined"
            label={t('register.phoneNumber')}
            inputProps={{ style: { textTransform: 'lowercase' } }}
          />

          <TextField
            error={error?.email !== ''}
            helperText={error?.email}
            value={email}
            onChange={(e) => setEmail(e.target.value.split(' ').join(''))}
            variant="outlined"
            label={t('register.email')}
            required
            inputProps={{ style: { textTransform: 'lowercase' } }}
          ></TextField>
          <TextField
            error={error?.category !== ''}
            helperText={error?.category}
            value={category}
            onChange={(e) => setCategory(e.target.value)}
            id="categ"
            select
            label={t('register.category')}
            inputProps={{ style: { textTransform: 'lowercase' } }}
          >
            {categories.map((category, index) => (
              <MenuItem key={index} value={category.name}>
                {category.name} {category.description}
              </MenuItem>
            ))}
          </TextField>
          <Box display="flex" justifyContent="flex-start">
            {failure ? (
              <Toast status={false} message={failedMessage}></Toast>
            ) : null}
            <Button
              variant="contained"
              component={Link}
              to={'/sign-in'}
              size="medium "
              style={styleSimple}
            >
              {t('register.cancelButton')}
            </Button>
            <Button
              variant="contained"
              onClick={createPost}
              size="medium "
              style={style}
            >
              {t('register.submit')}
            </Button>
          </Box>
        </Box>

        {/* <Button color="secondary" onClick={handleChangeLanguage}>
        Change Language
      </Button> */}
      </Container>
    </>
  )
}
