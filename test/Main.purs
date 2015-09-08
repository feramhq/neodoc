module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parser.Docopt as Docopt
import qualified Docopt.Parser.Lexer as Lexer
import qualified Data.String as Str

source =
"""
Naval-Fate. Usage: varies.

UsAgE: 

  naval_fate ask <question> | (foo|bar)
  naval_fate   run <command>

The program can be used in many ways.
Consider, for example:
Blah.

Options:

  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
"""

main = do
  let x = Docopt.docopt
            source
            "naval_fate"
  log $ show x
