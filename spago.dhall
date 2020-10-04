{ name = "homogeneous-records"
, dependencies =
    [ "record"
    , "typelevel-prelude"
    , "unfoldable"
    , "control"
    , "assert"
    , "lists"
    , "parallel"
    , "record-extra"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
