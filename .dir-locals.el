((nil
  . ((eval
      . (setq
         projectile-project-compilation-cmd
         (concat
          "docker run --rm -it"
          " --volume " (projectile-project-root) ":" (projectile-project-root)
          " refl-club-build "
          "cabal new-build")
         projectile-project-run-cmd
         (concat
          "docker run --rm --name refl.club"
          " --volume " (projectile-project-root) ":" (projectile-project-root)
          " refl-club-build "
          "sh")
         haskell-process-wrapper-function
         (lambda (argv)
           (append `("env" "-u" "GHC_PACKAGE_PATH") argv))))))
 (haskell-mode
  . ((haskell-process-type . cabal-new-repl)
     (haskell-mode-stylish-haskell-path . "ormolu")
     (haskell-mode-stylish-haskell-args . ("--ghc-opt TypeApplications"))
     (haskell-stylish-on-save . t))))
