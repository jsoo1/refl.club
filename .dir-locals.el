((haskell-mode . ((haskell-process-type . 'cabal-repl)
                  (haskell-mode-stylish-haskell-path . "ormolu")
                  (haskell-stylish-on-save . 't)))
 (nil . ((eval . (setq
                  projectile-compilation-command "cabal new-build"
                  haskell-process-wrapper-function (lambda (argv) (append '("env" "-u" "GHC_PACKAGE_PATH") argv)))))))
