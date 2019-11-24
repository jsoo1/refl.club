((nil
  . ((eval
      . (setq
         projectile-project-compilation-cmd
         (concat
          "docker run --rm -it"
          " --volume " (projectile-project-root) ":" (projectile-project-root)
          " refl.club-build "
          "cabal new-build")
         projectile-project-run-cmd
         (concat
          "docker run --rm --name refl.club"
          " --volume " (projectile-project-root) ":" (projectile-project-root)
          " refl.club-build "
          "sh")
         haskell-process-wrapper-function
         (lambda (argv)
           (append `("docker" "exec" "-it" "refl.club") argv))))))
 (haskell-mode
  . ((haskell-process-type . cabal-new-repl)
     (haskell-mode-stylish-haskell-path . "ormolu")
     (haskell-stylish-on-save . t))))
