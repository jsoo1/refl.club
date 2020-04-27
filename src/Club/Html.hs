{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Club.Html where

import Development.GitRev (gitHash)
import Lucid
import qualified Lucid.Base as Lucid

-- | Lucid script_ has no attributes.
script' :: Functor m => [Attribute] -> HtmlT m a -> HtmlT m a
script' =
  with (Lucid.makeElement "script")

css :: Applicative m => HtmlT m ()
css =
  link_
    [ href_ $ "/" <> $(gitHash) <> "-refl.css",
      rel_ "stylesheet",
      type_ "text/css"
    ]

prismCss :: Applicative m => HtmlT m ()
prismCss =
  link_
    [ rel_ "stylesheet",
      type_ "text/css",
      href_
        "https://cdnjs.cloudflare.com/ajax/libs/prism/1.20.0/\
        \themes/prism-solarizedlight.min.css"
    ]

prismJs :: Monad m => HtmlT m ()
prismJs = do
  script'
    [ src_
        "https://cdnjs.cloudflare.com/ajax/libs/prism/1.20.0/\
        \components/prism-core.min.js"
    ]
    ""
  script'
    [ src_
        "https://cdnjs.cloudflare.com/ajax/libs/prism/1.20.0/\
        \plugins/autoloader/prism-autoloader.min.js"
    ]
    ""
  script_ $ "Prism.plugins.autoloader.languages_path = '/" <> $(gitHash) <> "-grammars/';"
