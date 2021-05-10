{-# LANGUAGE TemplateHaskell #-}

module Exporter
  ( exportOrders,
    OrderExporter,
    --    runGoogleSheetExport,
  )
where

import Order
import PerifericoHelper
import Polysemy
import qualified Polysemy.Error as PE

data OrderExporter m a where
  ExportOrders :: [Order] -> OrderExporter m ()

makeSem ''OrderExporter

-- runGoogleSheetExport ::
--   Members '[Embed IO, PE.Error Text] m =>
--   Sem (OrderExporter ': m) a ->
--   Sem m a
-- runGoogleSheetExport = interpret @OrderExporter $ \case
--   ExportOrders orders -> do
--     let gdata = GData
--     -- login
--     -- orders update
--     pure ()
