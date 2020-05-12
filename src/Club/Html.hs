{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Club.Html where

import Club.Git (gitHead)
import Lucid
import qualified Lucid.Base as Lucid

data NavLocation = NavLocationAbout | NavLocationPosts
  deriving (Eq)

navBar :: Monad m => Maybe NavLocation -> HtmlT m ()
navBar curr =
  nav_ $ ul_ [style_ "display:flex;"] $ do
    skipToContent
    a_ (href_ "/" : [borderSolid | curr == Just NavLocationAbout]) $
      li_ [padLight] "About"
    verticalSep
    a_ (href_ "/posts" : [borderSolid | curr == Just NavLocationPosts]) $
      li_ [padLight] "Posts"

padLight :: Attribute
padLight = class_ "pad-light"

borderSolid :: Attribute
borderSolid = class_ "border-solid"

skipToContent :: Monad m => HtmlT m ()
skipToContent =
  a_ [href_ "#main", class_ "skip-to-content"] "Skip to content"

verticalSep :: Monad m => HtmlT m ()
verticalSep =
  span_
    [ style_ "margin-left:0.5rem;",
      style_ "margin-right:0.5rem;",
      style_ "background-color:black;",
      style_ "width:1px;"
    ]
    ""

-- | Lucid script_ has no attributes.
script' :: Functor m => [Attribute] -> HtmlT m a -> HtmlT m a
script' =
  with (Lucid.makeElement "script")

css :: Applicative m => HtmlT m ()
css =
  link_
    [ href_ $ "/" <> $(gitHead) <> "-refl.css",
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
  script_ $ "Prism.plugins.autoloader.languages_path = '/" <> $(gitHead) <> "-grammars/';"

cmuSerif :: Applicative m => HtmlT m ()
cmuSerif =
  link_
    [ rel_ "stylesheet",
      media_ "screen",
      href_ "https://fontlibrary.org/face/cmu-serif",
      type_ "text/css"
    ]

ccBySa :: Monad m => HtmlT m ()
ccBySa = p_ $ do
  "This work is licensed under a "
  a_
    [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/4.0/"]
    "Creative Commons Attribution-ShareAlike 4.0 International License"
