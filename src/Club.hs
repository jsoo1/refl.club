{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Club
  ( Club,
    clubApi,
    club,
  )
where

import About (About (..))
import AllPosts
import Control.Monad.Except (throwError)
import qualified Data.List as List
import Data.Post (Post (postMeta), PostMeta (postMetaSlug))
import Data.Text (Text)
import Lucid
import Post
import Servant
  ( (:<|>) (..),
    (:>) (..),
    Capture,
    Get,
    Proxy (..),
    Raw,
    Server,
    err404,
    serveDirectoryWebApp,
  )
import Servant.HTML.Lucid (HTML)

type Club =
  Get '[HTML] About
    :<|> "posts" :> Get '[HTML] AllPosts
    :<|> "post" :> Capture "slug" Text :> Get '[HTML] Post
    :<|> Raw

clubApi :: Proxy Club
clubApi = Proxy

club :: String -> Server Club
club staticDir =
  pure About
    :<|> pure allPosts
    :<|> ( \slug -> do
             let bySlug = (slug ==) . postMetaSlug . postMeta
             maybe (throwError err404) pure (lookupPost bySlug allPosts)
         )
    :<|> serveDirectoryWebApp staticDir
  where
    allPosts = AllPosts (fmap snd $(embedPosts "posts"))
