module Main (main) where

import Exporter
import Importer
import Order
import PerifericoHelper
import Polysemy
import qualified Polysemy.Error as PE
import qualified Polysemy.Reader as PR

main :: IO ()
main = do
  result <-
    importTest
      & runXLSFileImport
      & PR.runReader "/home/imgulyas/delivery.xlsx"
      & PE.runError @Text
      & runM

  case result of
    Left e -> print e
    Right recs -> for_ recs (putTextLn . show @Text)

update :: [Order] -> [Order] -> [Order]
update imported _ = imported

importDeduplicateUpdate :: Members '[OrderImporter, OrderExporter, OrderCache] m => Sem m ()
importDeduplicateUpdate = do
  imported <- importOrders
  fromCache <- readCache
  let updated = update imported fromCache
  cacheOrders updated
  exportOrders updated

importTest :: Members '[OrderImporter, Embed IO] m => Sem m [Order]
importTest = importOrders
