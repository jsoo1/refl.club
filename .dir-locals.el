((nil
  . ((eval
      . (setq
         projectile-project-compilation-cmd
         (concat
          "docker run -it"
          " --volume " (projectile-project-root) ":/src "
          "refl.club-build "
          "cabal new-build")
         haskell-process-wrapper-function
         (lambda (argv)
           (append
            `("env" "-u" "GHC_PACKAGE_PATH") argv))))))
 (haskell-mode
  . ((haskell-process-type . cabal-new-repl)
     (haskell-mode-stylish-haskell-path . "ormolu")
     (haskell-stylish-on-save . t))))
