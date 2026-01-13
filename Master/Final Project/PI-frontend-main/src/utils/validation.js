export const errorReport = {
  required: 'validation.required',
  len: 'validation.length',
  email: 'validation.email',
  'len=9|len=0': 'validation.length',
  'email-uniqueness': 'validation.emailUnique',
  'nif-uniqueness': 'validation.nifUnique',
  nifLenght: 'validation.nifLenght',
  phoneNumber: 'validation.phoneNumber',
  match: 'validation.match',
  passwordLenght: 'validation.passwordLength',
  passwordNumber: 'validation.passwordNumber',
  passwordChar: 'validation.passwordChar',
}

const formValidation = {
  firstName: [{ message: 'required', exp: /^.+$/ }],
  lastName: [{ message: 'required', exp: /^.+$/ }],
  email: [
    { message: 'required', exp: /^.+$/ },
    { message: 'email', exp: /^[\w\d._%+-]+@[a-zA-Z\d.-]+\.[a-zA-Z]{2,}$/ },
  ],
  phoneNumber: [
    { message: 'phoneNumber', exp: /^(?:\([\d-]+\) (?:\d{9})?)?$/ },
  ],
  nif: [{ message: 'nifLenght', exp: /^\d{9}$/ }],
  category: [{ message: 'required', exp: /^.+$/ }],
  password: [
    { message: 'required', exp: /^.+$/ },
    { message: 'passwordLenght', exp: /^.{8,}$/ },
    { message: 'passwordNumber', exp: /[0-9]/ },
    { message: 'passwordChar', exp: /[a-z]/ },
  ],
  confirmPassword: [{ message: 'required', exp: /^.+$/ }],
}

export const testField = (fieldName, fieldContent) => {
  let errorMessage = ''
  for (const test of formValidation[fieldName]) {
    if (!test.exp.test(fieldContent)) {
      errorMessage = test.message
      break
    }
  }
  return errorMessage
}
