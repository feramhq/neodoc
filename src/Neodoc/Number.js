export const readFloat = (str) => parseFloat(str)

export const readInt = (radix) => (str) => parseInt(str, radix)

export const isFinite = (n) => globalThis.isFinite(n)
