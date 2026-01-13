import { violet } from './color'

export const ThemeOptions = (mode) => {
  const lightPalette = {
    mode: 'light',
    bgApp: {
      bgColor: 'white',
      bgImage: undefined,
    },
    common: {
      black: 'black',
      white: 'white',
      darkGrey: '#545454', // grey[700]
      lightGrey: '#eeeeee', // grey[200]
      bgColor1: violet[400],
      bgColor2: '#FFB44F 40%',
      bgColor3: '#2490AC 75%',
      bgColor4: '#75A16E 90%',
      bgOpacity: 0.6,
    },
    primary: {
      ...violet,
    },
    secondary: {
      main: '#FFB44F',
    },
  }

  const darkPalette = {
    mode: 'dark',
    bgApp: {
      bgColor: '#121212',
      bgImage:
        'linear-gradient(rgba(255, 255, 255, 0.09),rgba(255, 255, 255, 0.09))',
    },
    common: {
      black: 'white',
      white: 'black',
      darkGrey: '#eeeeee', // grey[200]
      lightGrey: '#545454', // grey[700]
      bgColor1: violet[400],
      bgColor2: '#FFB44F 40%',
      bgColor3: '#2490AC 75%',
      bgColor4: '#75A16E 90%',
      bgOpacity: 0.6,
    },
    primary: {
      ...violet,
    },
    secondary: {
      main: '#FFB44F',
    },
  }

  return {
    spacing: 4,
    palette: mode === 'light' ? lightPalette : darkPalette,
    breakpoints: {
      values: {
        xs: 0,
        sm: 510,
        md: 900,
        ml: 1100,
        lg: 1200,
        xl: 1536,
      },
    },
    typography: {
      fontFamily: 'Poppins, sans-serif',
      title: {
        fontFamily: 'Poppins, sans-serif',
        fontWeight: 600,
        fontSize: '2rem',
      },
    },
    components: {
      MuiSkeleton: {
        defaultProps: {
          animation: 'wave',
        },
        styleOverrides: {
          root: {
            width: '100%',
            height: '50px',
            transform: 'scale(1, 0.70)',
          },
        },
      },
      MuiCssBaseline: {
        styleOverrides: `
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Black.ttf') format('truetype');
          font-weight: 900;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-BlackItalic.ttf') format('truetype');
          font-weight: 900;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-ExtraBold.ttf') format('truetype');
          font-weight: 800;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-ExtraBoldItalic.ttf') format('truetype');
          font-weight: 800;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Bold.ttf') format('truetype');
          font-weight: 700;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-BoldItalic.ttf') format('truetype');
          font-weight: 700;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-SemiBold.ttf') format('truetype');
          font-weight: 600;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-SemiBoldItalic.ttf') format('truetype');
          font-weight: 600;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Medium.ttf') format('truetype');
          font-weight: 500;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-MediumItalic.ttf') format('truetype');
          font-weight: 500;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Regular.ttf') format('truetype');
          font-weight: 400;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Italic.ttf') format('truetype');
          font-weight: 400;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Light.ttf') format('truetype');
          font-weight: 300;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-LightItalic.ttf') format('truetype');
          font-weight: 300;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-ExtraLight.ttf') format('truetype');
          font-weight: 200;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-ExtraLightItalic.ttf') format('truetype');
          font-weight: 200;
          font-style: italic;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-Thin.ttf') format('truetype');
          font-weight: 100;
          font-style: normal;
        }
        
        @font-face {
          font-family: 'Poppins';
          src: url('fonts/Poppins-ThinItalic.ttf') format('truetype');
          font-weight: 100;
          font-style: italic;
        }
        
        html, body {
          font-family: "Poppins, sans-serif";
        }
        `,
      },
    },
  }
}
