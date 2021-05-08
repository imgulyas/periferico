module Main (main) where

import PerifericoHelper
import Polysemy

main :: IO ()
main = print "hello world"

update :: [RawOrder] -> [RawOrder] -> [RawOrder]
update imported _ = imported

importDeduplicateUpdate :: Members '[RawOrderImporter, RawOrderExporter, RawOrderCache] m => Sem m ()
importDeduplicateUpdate = do
  imported <- importRawOrders
  fromCache <- readCache
  let updated = update imported fromCache
  cacheRawOrders updated
  exportRawOrders updated
