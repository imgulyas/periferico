{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AppState
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as L
import Exporter
import Graphics.UI.Threepenny.Core hiding (delete)
import Importer
import Order
import PerifericoHelper
import Polysemy
import qualified Polysemy.Error as PE
import qualified Polysemy.Reader as PR
import UI

data Secrets = Secrets
  { wooPublic :: Text,
    wooPrivate :: Text
  }
  deriving stock (Show)

deriveJSON defaultOptions ''Secrets

instance HasWooAccess Secrets where
  wooPublicKey = wooPublic
  wooPrivateKey = wooPrivate

initialState = AppState "initial ID input text"

main :: IO ()
main = do
  Just homeDir <- lookupEnv "HOME"
  let perifericoDir = homeDir <> "/.periferico"
  stateRef <- newIORef initialState

  --TODO: add .periferico dir existence check

  startGUI
    defaultConfig
      { jsPort = Just 5000,
        jsStatic = Just perifericoDir
      }
    (setupUI stateRef)

-- Just homeDir <- lookupEnv "HOME"
-- secretJson <- L.readFile $ homeDir <> "/.periferico"
-- let secrets :: Secrets
--     secrets = case (eitherDecode secretJson) of
--       Right s -> s
--       Left e -> error (toText e)

-- print secrets

-- result <-
--   importTest
--     & runXLSFileImport
--     & PR.runReader "/home/imgulyas/delivery.xlsx"
--     & PE.runError @Text
--     & runM

-- case result of
--   Left e -> print e
--   Right recs -> for_ recs (putTextLn . show @Text)

-- update :: [Order] -> [Order] -> [Order]
-- update imported _ = imported

-- importDeduplicateUpdate :: Members '[OrderImporter, OrderExporter, OrderCache] m => Sem m ()
-- importDeduplicateUpdate = do
--   imported <- importOrders
--   fromCache <- readCache
--   let updated = update imported fromCache
--   cacheOrders updated
--   exportOrders updated

-- importTest :: Members '[OrderImporter, Embed IO] m => Sem m [Order]
-- importTest = importOrders
