(use-modules
 (ghc-aws-lambda-haskell-runtime)
 (ghc-lucid)
 (gnu packages compression)
 (gnu packages haskell-apps)
 (gnu packages haskell-web)
 (ormolu))

`(,ghc-aeson
  ,ghc-lucid
  ,ghc-aws-lambda-haskell-runtime
  ,hlint
  ,lzlib
  ,zlib
  ,ormolu)
