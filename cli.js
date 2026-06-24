const neodoc = require('./index.js')

const helpText = `
Usage: neodoc [options]

Options:
  --dont-exit  Do not exit upon error or when parsing --help or --version.
  --env  Override process.env
  --argv  Override process.argv
  --options-first  Parse until the first command or <positional> argument, then collect the rest into an array, given the help indicates another, repeatable, positional argument.
  --smart-options  Enable parsing groups that "look like" options as options.
  --stop-at  Stop parsing at the given options, i.e. [ -n ].
  --require-flags  Require flags be present in the input. In neodoc, flags are optional by default and can be omitted.
  --lax-placement  Relax placement rules. Positionals and commands are no longer solid anchors.
  --version-flags  An array of flags that trigger the special version behavior: Print the program version and exit with code 0.
  --version  The version to print for the special version behavior.
  --help-flags  An array of flags that trigger the special help behavior: Print the full program help text and exit with code 0.
  --repeatable-options  Allow options to be repeated even if the spec does not explicitly allow this.
  --transforms-presolve  An array of functions to be called prior to "solving" the input.
  --transforms-postsolve  An array of functions to be called after "solving" the input, just prior to passing the spec to the arg-parser.
  --allow-unknown  Collect unknown options under a special key ? instead of failing.

Examples:
  neodoc --options-first < help.txt
`

const navalHelpText = `
Naval Fate.

Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
`

const result = neodoc.run(navalHelpText, {
  laxPlacement: true,
  argv: ['ship', 'new', 'foobar'],
  // argv: ['mine', 'set', '1', '2', '--moored'],
})

console.info(result)
