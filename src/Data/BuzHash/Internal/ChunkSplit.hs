module Data.BuzHash.Internal.ChunkSplit
  ( ChunkSplit (..)
  , splitByteString
  , mergeChunks
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid

data ChunkSplit a = Chunk      !a
                  | SplitAfter !a
  deriving (Eq, Ord, Show, Read)

instance Functor ChunkSplit where
  fmap f (Chunk x)      = Chunk (f x)
  fmap f (SplitAfter x) = SplitAfter (f x)

splitByteString :: ByteString -> [Int] -> [ChunkSplit ByteString]
splitByteString bs (n:ns) =
  case BS.splitAt n bs of
    (bsPre, bsPost) -> SplitAfter bsPre : splitByteString bsPost ns
splitByteString bs [] | not (BS.null bs) = Chunk bs : []
                      | otherwise        = []

mergeChunks :: Monoid m => [ChunkSplit m] -> [ChunkSplit m]
mergeChunks (c@(SplitAfter _):cs) = c : mergeChunks cs
mergeChunks (Chunk x:Chunk y:cs) = mergeChunks (Chunk (x <> y):cs)
mergeChunks (Chunk x:SplitAfter y:cs) = SplitAfter (x <> y) : mergeChunks cs
mergeChunks cs = cs
