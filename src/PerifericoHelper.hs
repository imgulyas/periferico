{- |
Copyright: (c) 2021 Imre Gulyas
SPDX-License-Identifier: MIT
Maintainer: Imre Gulyas <imgulyas@gmail.com>

Small util for Periferico
-}

module PerifericoHelper
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
