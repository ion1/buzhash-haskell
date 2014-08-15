module Main (main) where

import Test.Tasty

import qualified BuzHash
import qualified Table

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "buzhash"
    [ BuzHash.tests
    , Table.tests
    ]
