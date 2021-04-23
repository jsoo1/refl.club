{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of refl.club - a personal website and blog
-- Copyright (C) 2020 John Soo <jsoo1@asu.edu>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module About where

import Club.Git (gitHead)
import qualified Club.Html as Club
import Lucid

data About = About

instance ToHtml About where

  toHtmlRaw = toHtml

  toHtml _ =
    doctypehtml_ $ do
      head_ $ do
        title_ "John Soo"
        Club.waitOnFonts
        Club.css
      body_ $ do
        div_ [style_ "padding:0.5rem;"]
          $ Club.navBar
          $ Just Club.NavLocationAbout
        section_ [id_ "main"] $ do
          h1_ "John Soo"
          p_ "Functional programming enthusiast"
          p_ "Host: Orange Combinator, aspiring proof engineer"
          a_ [href_ "/john-soo.asc"] "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"
          nav_ $ ul_ $ do
            h2_ [id_ "profiles"] "Profiles"
            a_ [href_ $ "/" <> $(gitHead) <> "-john-soo-resume.pdf"] $
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
          section_ $ do
            h3_ $ do
              a_ [href_ "https://guix.gnu.org"] "Guix"
              "(functional package manager)"
            p_
              "Ecosystem support, functional languages"
          section_ $ do
            h3_ $ do
              a_ [href_ "https://github.com/jsoo1/guix-channel"] "Guix Channel"
              "(extra packages for Guix)"
            p_ "To upstream after incubation"
          section_ $ do
            h3_ $ a_ [href_ "https://github.com/jsoo1/dotfiles"] "Dotfiles"
            p_ "Guix, Alacritty, Tmux, Emacs, and Xmonad"
        section_ [id_ "implementation"] $ do
          h2_ "Implementation"
          p_ $ do
            a_
              [ style_ "margin-right:0.25rem;",
                href_ "https://github.com/jsoo1/refl.club"
              ]
              "Source"
            span_ [style_ "word-break:break-word;"]
              $ toHtml
              $ " (commit " <> $(gitHead) <> ")"
          p_ $ do
            a_ [href_ "https://www.haskell.org"] "Haskell"
            ","
            a_
              [ href_ "https://docs.servant.dev/",
                style_ "margin-left:0.25rem;"
              ]
              "Servant"
          p_ $ do
            "Org files:"
            a_
              [ href_ "https://www.orgmode.org",
                style_ "margin-left:0.25rem;"
              ]
              "org-mode"
            ","
            a_
              [ href_ "https://github.com/fosskers/org-mode",
                style_ "margin-left:0.25rem;"
              ]
              "parser"
          p_ $ do
            "Syntax:"
            a_
              [ style_ "margin-left:0.25rem;",
                href_ "https://github.com/PrismJS/prism/"
              ]
              "Prism"
          p_ $ do
            "Monospace:"
            a_
              [ style_ "margin-left:0.25rem;",
                href_ "https://typeof.net/Iosevka/"
              ]
              "Iosevka"
