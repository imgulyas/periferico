{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: (c) 2021 Imre Gulyas
-- SPDX-License-Identifier: MIT
-- Maintainer: Imre Gulyas <imgulyas@gmail.com>
--
-- Small util for Periferico
module PerifericoHelper
  ( OrderCache (..),
    cacheOrders,
    readCache,
  )
where

import Domain
import Polysemy

data OrderCache m a where
  CacheOrders :: [Order] -> OrderCache m ()
  ReadCache :: OrderCache m [Order]

makeSem ''OrderCache
