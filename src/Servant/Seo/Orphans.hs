{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Seo.Orphans where

import Lucid (ToHtml)
import Servant (Get, Headers, Proxy (..))
import Servant.HTML.Lucid (HTML)
import Servant.Seo (HasSitemap (..), SitemapInfo (..))

-- | @Get '[HTML]@ enables sitemap for particular API branch.
instance {-# OVERLAPPING #-} ToHtml a => HasSitemap (Get '[HTML] a) where
  toSitemapInfo _ = pure (SitemapInfo [mempty] (Just ()))
