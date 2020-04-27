{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module About where

import qualified Club.Html as Club
import Development.GitRev (gitHash)
import Lucid

data About = About

instance ToHtml About where

  toHtmlRaw = toHtml

  toHtml _ =
    doctypehtml_ $ do
      head_ $ do
        title_ "John Soo"
        Club.css
      body_ [style_ "padding:3rem;"] $ do
        section_ [id_ "title"] $ do
          h1_ "John Soo"
          p_ "Software Engineer and functional programming enthusiast."
          p_ "Host of the Orange Combinator meetup and aspiring proof engineer."
          p_ "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"
          nav_ $ ul_ $ do
            a_ [href_ $ "/" <> $(gitHash) <> "-john-soo-resume.pdf"] $
              li_ "Resume"
            a_ [href_ "/posts"] $
              li_ "Posts"
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
          section_ $ do
            h3_ $ do
              a_ [href_ "https://guix.gnu.org"] "Guix"
              "(functional package manager extended in scheme)"
            p_
              "Currently working on ecosystem support for \
              \Haskell, Coq, Cedille and Rust"
          section_ $ do
            h3_ $ do
              a_ [href_ "https://github.com/jsoo1/guix-channel"] "Guix Channel"
              "(extra packages for Guix)"
            p_ "These packages tend to go upstream after incubation."
          section_ $ do
            h3_ $ a_ [href_ "https://github.com/jsoo1/dotfiles"] "Dotfiles"
            p_
              "Assortment of configurations for Guix, Alacritty, \
              \Tmux, Emacs, and Xmonad"
        section_ [id_ "implementation"] $ do
          h2_ "Implementation"
          p_ $ do
            "This is a very simple Servant application ("
            a_
              [ style_ "margin-right:0.25rem",
                href_ "https://github.com/jsoo1/refl.club"
              ]
              "source"
            ", commit " <> $(gitHash) <> ")"
          p_ $ do
            "Org file parsing and formatting with"
            a_
              [ href_ "https://github.com/fosskers/org-mode",
                style_ "margin-left:0.25rem;"
              ]
              "org-mode"
          p_ $ do
            "Syntax highlighting by"
            a_
              [ style_ "margin-left:0.25rem",
                href_ "https://github.com/PrismJS/prism/"
              ]
              "Prism"
