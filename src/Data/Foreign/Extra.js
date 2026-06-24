// module Data.Foreign.Extra

export const _isTruthy = (value) => !!value

const undefinedValue = undefined
export { undefinedValue as undefined }

export const toString = (value) => value.toString()
