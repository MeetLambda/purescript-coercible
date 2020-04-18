{
    sources = [ "src/**/*.purs", "test/**/*.purs" ],
    name = "purescript-coercible",
    dependencies =  [ "strings", "lists", "effect", "console" ],
    packages = ./packages.dhall
}
