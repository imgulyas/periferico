module Main (main) where

import Importer
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
    Right recs -> for_ recs print

update :: [RawOrder] -> [RawOrder] -> [RawOrder]
update imported _ = imported

importDeduplicateUpdate :: Members '[RawOrderImporter, RawOrderExporter, RawOrderCache] m => Sem m ()
importDeduplicateUpdate = do
  imported <- importRawOrders
  fromCache <- readCache
  let updated = update imported fromCache
  cacheRawOrders updated
  exportRawOrders updated

importTest :: Members '[RawOrderImporter, Embed IO] m => Sem m [RawOrder]
importTest = importRawOrders
