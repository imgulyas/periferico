{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: (c) 2021 Imre Gulyas
-- SPDX-License-Identifier: MIT
-- Maintainer: Imre Gulyas <imgulyas@gmail.com>
--
-- Small util for Periferico
module PerifericoHelper
  ( RawOrder (..),
    RawOrderExporter (..),
    RawOrderCache (..),
    exportRawOrders,
    cacheRawOrders,
    readCache,
  )
where

import Polysemy

-- Order No	Order Date	Order Status	Delivery Details	Delivery Status	Billing Address	Shipping Address	Products	Total	Payment Method	Customer Note	kiszállítási határidő
data RawOrder = RawOrder
  { orderNo :: Text,
    orderDate :: Text,
    orderStatus :: Text,
    deliveryDetails :: Text,
    deliveryStatus :: Text,
    billingAddress :: Text,
    shippingAddress :: Text,
    products :: Text,
    total :: Text,
    paymentMethod :: Text,
    customerNote :: Text,
    deliveryDeadline :: Text
  }
  deriving stock (Show, Eq)

data RawOrderExporter m a where
  ExportRawOrders :: [RawOrder] -> RawOrderExporter m ()

makeSem ''RawOrderExporter

data RawOrderCache m a where
  CacheRawOrders :: [RawOrder] -> RawOrderCache m ()
  ReadCache :: RawOrderCache m [RawOrder]

makeSem ''RawOrderCache
