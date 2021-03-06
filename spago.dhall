{ name = "neodoc"
, dependencies =
    [ "aff"
    , "argonaut-codecs"
    , "argonaut-core"
    , "argonaut-generic"
    , "arrays"
    , "assert"
    , "bifunctors"
    , "console"
    , "control"
    , "datetime"
    , "debug"
    , "effect"
    , "either"
    , "exceptions"
    , "foreign"
    , "free"
    , "generics-rep"
    , "globals"
    , "integers"
    , "lists"
    , "maybe"
    , "node-fs"
    , "node-process"
    , "nonempty"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "spec"
    , "strings"
    , "template-strings"
    , "transformers"
    , "unsafe-coerce"
    , "yarn"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
