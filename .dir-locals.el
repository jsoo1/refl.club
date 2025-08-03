((nil . (compilation-command "nix build -L --no-link --print-out-paths"))
 (haskell-mode
  . ((haskell-process-type . cabal-repl)
     (haskell-mode-stylish-haskell-path . "ormolu")
     (haskell-mode-stylish-haskell-args . ("--ghc-opt" "TypeApplications"))
     (haskell-stylish-on-save . t))))
