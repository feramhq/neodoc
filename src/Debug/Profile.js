// module Debug.Profile

export const _ENABLE_PROFILING_ =
  process.env['NEODOC_ENABLE_PROFILE'] == '1' ||
  process.env['NEODOC_ENABLE_PROFILE'] == 'true'

export const timerStart = () => process.hrtime()

export const timerEnd = (start) => () => {
  const hrTime = process.hrtime(start)
  return hrTime[0] * 1000 + hrTime[1] / 1000000
}
