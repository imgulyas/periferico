{-# LANGUAGE TemplateHaskell #-}

module Importer
  ( importOrders,
    OrderImporter,
    runImportPure,
    runXLSFileImport,
    runWooRestImporter,
  )
where

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import Polysemy
import qualified Polysemy.Error as PE
import qualified Polysemy.Reader as PR

import Order

data OrderImporter m a where
  ImportOrders :: OrderImporter m [Order]

makeSem ''OrderImporter

testOrder :: Order
testOrder =
  Order
    { _orderId = "#1",
      _orderDate = "date",
      _orderStatus = "orderStatus",
      _deliveryDetails = "deliveryDetails",
      _deliveryStatus = "deliveryStatus",
      _billingAddress = "billingAddress",
      _shippingAddress = "shippingAddress",
      _products = "products",
      _total = "total",
      _paymentMethod = "paymentMethod",
      _customerNote = "customerNote",
      _deliveryDeadline = "deliveryDeadline"
    }

runImportPure :: Sem (OrderImporter ': m) a -> Sem m a
runImportPure = interpret $ \case
  ImportOrders -> pure [testOrder]

readOrder :: Worksheet -> Int -> Order
readOrder ws row =
  let textAt :: Int -> Text
      textAt col =
        case ws ^? ixCell (row, col) . cellValue . _Just of
          Just (CellText t) -> t
          _ -> "" -- "no cell text in (" <> show row <> ", " <> show col <> ")"
   in Order (textAt 1) (textAt 2) (textAt 3) (textAt 4) (textAt 5) (textAt 6) (textAt 7) (textAt 8) (textAt 9) (textAt 10) (textAt 11) (textAt 12)

runXLSFileImport ::
  Members '[Embed IO, PE.Error Text] m =>
  Sem (OrderImporter ': m) a ->
  Sem (PR.Reader FilePath ': m) a
runXLSFileImport = reinterpret @OrderImporter @(PR.Reader FilePath) $ \case
  ImportOrders -> do
    path <- PR.ask
    bytestring <- embed $ L.readFile path
    let maybeWorksheet = toXlsx bytestring ^? ixSheet "Worksheet"
    case maybeWorksheet of
      Just worksheet ->
        let lastRow = length $ takeWhile isJust $ (\row -> worksheet ^? ixCell (row, 1) . cellValue . _Just) <$> [2 ..]
         in pure $ map (readOrder worksheet) [2 .. lastRow]
      Nothing -> PE.throw @Text "no worksheet with name 'Worksheet'"



runWooRestImporter ::
  Members '[Embed IO, PE.Error Text] m =>
  Sem (OrderImporter ': m) a ->
  Sem m a
runWooRestImporter = interpret @OrderImporter $ \case
  ImportOrders -> pure [testOrder]
