/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{html,js}"],
  theme: {
    container: {
      center: true,
    },
    extend: {
      colors: {
        "musi-gold": "#B18E36",
        "musi-green": "#36B151",
        "musi-blue": "#3659B1",
        "musi-red": "#B13696"
      },
      keyframes: {
        fadeIn: {
          '0%': { opacity: 0 },
          '100%': { opacity: 1 },
        }
      },
      animation: {
        fadeIn: 'fadeIn 100ms ease-in',
      }
    },
  },
  plugins: [],
}

