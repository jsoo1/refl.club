{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Club.Html where

import Development.GitRev (gitHash)
import Lucid

css :: Applicative m => HtmlT m ()
css =
  link_
    [ href_ $ "/" <> $(gitHash) <> "-refl.css",
      rel_ "stylesheet",
      type_ "text/css"
    ]
