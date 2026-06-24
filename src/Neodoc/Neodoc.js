// module Neodoc

import fs from 'fs'
import path from 'path'

/**
 * Try and detect the version as indicated in the package.json neighbouring
 * the main module. Walks up the parent directories from the entry script in
 * search of a `package.json`.
 */
export const readPkgVersionImpl = (Just) => (Nothing) => () => {
  try {
    let dir = path.dirname(process.argv[1] || process.cwd())
    for (let i = 0; i < 50; i++) {
      const p = path.join(dir, 'package.json')
      if (fs.existsSync(p)) {
        return Just(JSON.parse(fs.readFileSync(p, 'utf8')).version)
      }
      const parent = path.dirname(dir)
      if (parent === dir) break
      dir = parent
    }
    return Nothing
  } catch (e) {
    return Nothing
  }
}
