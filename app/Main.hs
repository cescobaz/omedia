{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Service

main :: IO ()
main = Service.run Options { port = 3000, homePath = "/Users/cescobaz/omedia" }
