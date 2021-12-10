{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "ordered-collections"
  , "parallel"
  , "point-free"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "stringutils"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
