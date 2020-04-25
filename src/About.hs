{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module About where

import Development.GitRev (gitHash)
import Lucid

data About = About

instance ToHtml About where

  toHtmlRaw = toHtml

  toHtml _ =
    doctypehtml_ $ do
      head_ $ do
        title_ "John Soo"
        link_
          [ href_ $ "/" <> $(gitHash) <> "-refl.css",
            rel_ "stylesheet",
            type_ "text/css"
          ]
      body_ $ do
        section_ [id_ "title"] $ do
          h1_ "John Soo"
          p_ "Software Engineer and functional programming enthusiast."
          p_ "Host of the Orange Combinator meetup and aspiring proof engineer."
          p_ "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"
          nav_ $ ul_ $ do
            a_ [href_ $ "/" <> $(gitHash) <> "-john-soo-resume.pdf"] $
              li_ "Resume"
            a_ [href_ "mailto:jsoo1@asu.edu"] $
              li_ "jsoo1@asu.edu"
            a_ [href_ "https://github.com/jsoo1"] $
              li_ "github.com/jsoo1"
            a_ [href_ "https://twitter.com/jsoo1"] $
              li_ "twitter.com/jsoo1"
            a_ [href_ "https://meetup.com/orange-combinator"] $
              li_ "meetup.com/orange-combinator"
        section_ [id_ "activity"] $ do
          h2_ "Activity"
          ul_ $ do
            li_ $ do
              p_ $ do
                a_ [href_ "https://guix.gnu.org"] "Guix"
                "(functional package manager extended in scheme)"
              p_
                [style_ "padding-left:0.5rem"]
                "Currently working on ecosystem support for \
                \Haskell, Coq, Cedille and Rust"
            li_ $ do
              p_ $ do
                a_ [href_ "https://github.com/jsoo1/guix-channel"] "Guix Channel"
                "(extra packages for Guix)"
              p_
                [style_ "padding-left:0.5rem"]
                "These packages tend to go upstream after incubation."
            li_ $ do
              a_ [href_ "https://github.com/jsoo1/dotfiles"] "Dotfiles"
              p_
                [style_ "padding-left:0.5rem"]
                "Assortment of configurations for Guix, Alacritty, \
                \Tmux, Emacs, and Xmonad."
        section_ [id_ "implementation"] $ do
          h2_ "Implementation"
          p_ $ do
            "This is a very simple Servant application"
            a_
              [ style_ "margin-left:0.25rem",
                href_ "https://github.com/jsoo1/refl.club"
              ]
              "(Source)"
          p_ $ "Commit " <> $(gitHash)
