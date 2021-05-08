{-# LANGUAGE TemplateHaskell #-}

module Importer
  ( importRawOrders,
    RawOrderImporter (..),
    runImportPure,
    runXLSFileImport,
  )
where

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import PerifericoHelper (RawOrder (..))
import Polysemy
import qualified Polysemy.Error as PE
import qualified Polysemy.Reader as PR

data RawOrderImporter m a where
  ImportRawOrders :: RawOrderImporter m [RawOrder]

makeSem ''RawOrderImporter

testOrder =
  RawOrder
    { orderNo = "#1",
      orderDate = "date",
      orderStatus = "orderStatus",
      deliveryDetails = "deliveryDetails",
      deliveryStatus = "deliveryStatus",
      billingAddress = "billingAddress",
      shippingAddress = "shippingAddress",
      products = "products",
      total = "total",
      paymentMethod = "paymentMethod",
      customerNote = "customerNote",
      deliveryDeadline = "deliveryDeadline"
    }

runImportPure :: Sem (RawOrderImporter ': m) a -> Sem m a
runImportPure = interpret $ \case
  ImportRawOrders -> pure [testOrder]

readRawOrder :: Worksheet -> Int -> RawOrder
readRawOrder ws row =
  let textAt :: Int -> Text
      textAt col =
        case ws ^? ixCell (row, col) . cellValue . _Just of
          Just (CellText t) -> t
          _ -> "" -- "no cell text in (" <> show row <> ", " <> show col <> ")"
   in RawOrder (textAt 1) (textAt 2) (textAt 3) (textAt 4) (textAt 5) (textAt 6) (textAt 7) (textAt 8) (textAt 9) (textAt 10) (textAt 11) (textAt 12)

runXLSFileImport ::
  Members '[Embed IO, PE.Error Text] m =>
  Sem (RawOrderImporter ': m) a ->
  Sem (PR.Reader FilePath ': m) a
runXLSFileImport = reinterpret @RawOrderImporter @(PR.Reader FilePath) $ \case
  ImportRawOrders -> do
    path <- PR.ask
    bytestring <- embed $ L.readFile path
    let maybeWorksheet = toXlsx bytestring ^? ixSheet "Worksheet"
    case maybeWorksheet of
      Just worksheet ->
        let lastRow = length $ takeWhile isJust $ (\row -> worksheet ^? ixCell (row, 1) . cellValue . _Just) <$> [2 ..]
         in pure $ map (readRawOrder worksheet) [2 .. lastRow]
      Nothing -> PE.throw @Text "no worksheet with name 'Worksheet'"
