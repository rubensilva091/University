import i18n from 'i18next'
import { initReactI18next } from 'react-i18next'
import LanguageDetector from 'i18next-browser-languagedetector'
import enTranslation from '../locales/en.json'
import ptTranslation from '../locales/pt.json'

export const fallbackLng = 'pt'

export const supportedLanguages = {
  pt: { label: 'Português', flagCode: 'pt' },
  en: { label: 'English', flagCode: 'gb' },
}

i18n
  .use(LanguageDetector)
  .use(initReactI18next)
  .init({
    fallbackLng: fallbackLng,
    resources: {
      pt: { ...ptTranslation },
      en: { ...enTranslation },
    },
    supportedLngs: Object.keys(supportedLanguages),
    detection: {
      order: ['localStorage', 'navigator'],
      caches: ['localStorage'],
    },
  })
