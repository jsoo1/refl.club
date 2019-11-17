(define-module (refl-club)
  #:use-module (ghc-aws-lambda-haskell-runtime)
  #:use-module (ghc-lucid)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz))

(define-public refl-club
  (package
    (name "refl-club")
    (version "389420f8f0d29637cb26723a1c1d033ee88ac898")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoo1/refl.club")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cld774di9zv9nc25rs1g945l0835xrxrc67y7g69vvxzn6g4v0k"))))
    (inputs
     `(("ghc-aws-lambda-haskell-runtime" ,ghc-aws-lambda-haskell-runtime)
       ("ghc-lucid" ,ghc-lucid)))
    (build-system haskell-build-system)
    (home-page "refl.club")
    (synopsis "My personal website")
    (description "My personal website")
    (license license:gpl3)))
