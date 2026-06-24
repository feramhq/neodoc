// module Neodoc.Spec.Lexer

/**
 * trim the description section in order to make lexing faster.
 */
export const trimDescSection = (str) => {
  str = '\n' + str // note: ensure to capture match on first line
  const regex = /(.*(--?\S* *(((?!\[default *:|\[env *:)\S*) *(\.{3})?)?)|(^\s*)?\[(default|env): ("(?:[^"\\]|\\.)*"\s*|.*)*\])/gmi
  let out = ''
  let m
  while ((m = regex.exec(str)) !== null) {
    if (m.index === regex.lastIndex) {
      regex.lastIndex++
    }

    if (m[0][0] !== '[') {
      out += '\n'
    } else {
      out += ' '
    }

    out += m[0]
  }
  return out
}
