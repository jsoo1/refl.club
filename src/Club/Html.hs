{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Club.Html where

import Development.GitRev (gitHash)
import Lucid
import qualified Lucid.Base as Lucid

cmuSerif :: Applicative m => HtmlT m ()
cmuSerif =
  link_
    [ rel_ "stylesheet",
      media_ "screen",
      href_ "https://fontlibrary.org/face/cmu-serif",
      type_ "text/css"
    ]

verticalSep :: Applicative m => HtmlT m a -> HtmlT m a
verticalSep =
  div_
    [ style_ "margin-left:0.5rem;",
      style_ "margin-right:0.5rem;",
      style_ "background-color:black;",
      style_ "width:1px;"
    ]

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
