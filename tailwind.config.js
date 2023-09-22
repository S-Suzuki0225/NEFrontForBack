/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{elm,ts}", "./styles/**/*.{scss,css}", "./index.html"],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
};
