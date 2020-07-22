((nil
  . ((eval
      . (setq
         projectile-project-compilation-cmd
         "env -u GHC_PACKAGE_PATH cabal new-build"
         projectile-project-run-cmd
         "env -u GHC_PACKAGE_PATH PORT=4000 cabal new-run exe:refl-club -- .static"
         haskell-process-wrapper-function
         (lambda (argv)
           (append `("env" "-u" "GHC_PACKAGE_PATH") argv))))))
 (makefile-mode . ((tab-width . 4)))
 (haskell-mode
  . ((haskell-process-type . cabal-repl)
     (haskell-mode-stylish-haskell-path . "ormolu")
     (haskell-mode-stylish-haskell-args . ("--ghc-opt TypeApplications"))
     (haskell-stylish-on-save . t))))
