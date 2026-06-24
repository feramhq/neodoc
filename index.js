var neodocLib = require('./lib.js')

module.exports.run = function (helpOrSpec, options = {}) {
  options.argv =
    Array.isArray(options.argv)
      ? options.argv
      : (process.argv || []).slice(2)
  options.env = Object.entries(
    options.env && typeof options.env == 'object'
      ? options.env
      : (process.env || {})
    )

  var transforms = options.transforms || {}
  var presolve = Array.isArray(transforms.presolve) ? transforms.presolve : []
  var postsolve = Array.isArray(transforms.postsolve) ? transforms.postsolve : []

  var result

  if (helpOrSpec) {
    if (typeof helpOrSpec === 'string') {
      result = neodocLib.runStringJs(helpOrSpec)(options)(presolve)(postsolve)
    }
    else if (typeof helpOrSpec === 'object') {
      result = neodocLib.runSpecJs(helpOrSpec)(options)
    }
  }

  if (!result) {
    throw new Error('Either provide a help text or a command specification')
  }

  var dontExit = options.dontExit === true

  // An error result is an object carrying a rendered `error` message.
  if (!Array.isArray(result) && result.error) {
    if (dontExit) {
      throw new Error(result.error)
    }
    console.error(result.error)
    return process.exit(1)
  }

  var args = Object.fromEntries(result)

  // `.help` / `.version` are set when a help or version flag was triggered.
  // Unless the caller opted out via `dontExit`, print them and exit.
  if (!dontExit) {
    if ('.help' in args) {
      console.log(args['.help'])
      return process.exit(0)
    }
    if ('.version' in args) {
      console.log(args['.version'])
      return process.exit(0)
    }
  }

  return args
}
