import Test.DocTest
main = doctest ["-isrc", "-XHaskell2010", "src/Codec/Candid.hs", "--fast"]
