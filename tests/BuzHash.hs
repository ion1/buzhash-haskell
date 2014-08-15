{-# OPTIONS_GHC -fno-warn-orphans #-}

module BuzHash (tests) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Function
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.BuzHash
import Data.BuzHash.Internal.ChunkSplit
import Data.BuzHash.Internal.ListAnd

instance Arbitrary BuzHash where
  arbitrary = do
    Positive windowSize <- arbitrary
    NonNegative chunkMinSize <- arbitrary
    Long (Positive chunkMaxSize) <- arbitrary
    maskBits <- arbitrary `suchThat` (\n -> n > 0 && n <= 31)
    return (buzHash windowSize chunkMinSize chunkMaxSize maskBits)

newtype Long a = Long a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Long a) where
  arbitrary = Long <$> sized (\s -> resize (s*100) arbitrary)
  shrink (Long a) = map Long (shrink a)

tests :: TestTree
tests =
  testGroup "Data.BuzHash"
    [ testProperty "buzHashList output matches the input" $
        \bh (Long bs) ->
          forAll (randomChunking bs) $ \bsC ->
            let bs' = BSL.fromChunks . map flattenCS . toList
                    $ buzHashList bh bsC
                flattenCS (Chunk      c) = c
                flattenCS (SplitAfter c) = c
            in  BSL.fromStrict bs === bs'

    , testProperty "buzHashList matches buzHashListInefficient" $
        \bh (Long bs) ->
          forAll (randomChunking bs) $ \bsC ->
            buzHashList bh bsC === buzHashListInefficient bh bsC

    , testProperty "buzHashList does not mind different input chunkings" $
        \bh (Long bs) ->
          forAll (randomChunking bs) $ \bsCA ->
            forAll (randomChunking bs) $ \bsCB ->
              let (cssA, finA) = listAndToList (buzHashList bh bsCA)
                  (cssB, finB) = listAndToList (buzHashList bh bsCB)
              in  conjoin [ cssA `chunkSplitsMatch` cssB
                          , finA === finB
                          ]
    ]

randomChunking :: ByteString -> Gen [ByteString]
randomChunking bs 
  | BS.null bs = return []
  | otherwise  = do
      (ls, rs) <- randomSplit bs
      (ls:) <$> randomChunking rs

randomSplit :: ByteString -> Gen (ByteString, ByteString)
randomSplit bs = do
  NonNegative n <- arbitrary
  return (BS.splitAt n bs)

-- Will clone the BuzHash state for each input list element.
buzHashListInefficient :: BuzHash -> [ByteString]
                       -> ListAnd BuzHash (ChunkSplit ByteString)
buzHashListInefficient bhOrig (bs:bss) = go (buzHashBS bhOrig bs)
  where go (Cons cs css) = Cons cs (go css)
        go (End bh)      = buzHashListInefficient bh bss
buzHashListInefficient bh [] = End bh

chunkSplitsMatch :: [ChunkSplit ByteString] -> [ChunkSplit ByteString]
                 -> Property
chunkSplitsMatch = (===) `on` mergeChunks . (fmap . fmap) BSL.fromStrict
