module Data.BuzHash
  ( ListAnd (..)
  , ChunkSplit (..)
  , BuzHash
  , BuzHashRepr (..)
  , buzHash
  , buzHashBS
  , buzHashBSL
  , buzHashList
  , buzHashRepr
  , buzTable
  ) where

import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import Data.Word
import Control.Applicative
import System.IO.Unsafe

import Data.BuzHash.Internal.ListAnd
import Data.BuzHash.Internal.ChunkSplit
import Data.BuzHash.IO (BuzHashRepr (..))
import qualified Data.BuzHash.IO as BHIO

newtype BuzHash = BuzHash BHIO.BuzHashIO

instance Eq BuzHash where
  (==) = (==) `on` buzHashRepr

instance Show BuzHash where
  showsPrec p bh = showParen (p > 10)
                 $ showString "BuzHash " . showsPrec 11 (buzHashRepr bh)

buzHash :: Integer  -- ^ window size, divided by 32
        -> Integer  -- ^ minimum chunk size
        -> Integer  -- ^ maximum chunk size
        -> Integer  -- ^ number of mask bits
        -> BuzHash
buzHash windowSize chunkMinSize chunkMaxSize maskBits = unsafePerformIO $ do
  bhio <- BHIO.newIO windowSize chunkMinSize chunkMaxSize maskBits
  return (BuzHash bhio)
{-# NOINLINE buzHash #-}

buzHashBS :: BuzHash -> ByteString -> ListAnd BuzHash (ChunkSplit ByteString)
buzHashBS bh bs = buzHashList bh [bs]

buzHashBSL :: BuzHash -> BSL.ByteString
           -> ListAnd BuzHash (ChunkSplit ByteString)
buzHashBSL bh bsl = buzHashList bh (BSL.toChunks bsl)

buzHashList :: BuzHash -> [ByteString]
            -> ListAnd BuzHash (ChunkSplit ByteString)
buzHashList (BuzHash bhioOrig) input = unsafePerformIO $ do
  -- Clone the BH state, do not mutate the original.
  bhio <- BHIO.cloneIO bhioOrig
  go bhio input
  where
    go bhio (bs:bss) = unsafeInterleaveIO $ do
      chunkSplits <- BHIO.processIO bhio bs
      prepend chunkSplits <$> go bhio bss

    -- After all the mutations to the clone have occurred, return it never to
    -- be mutated again.
    go bhio [] = return (End (BuzHash bhio))

    prepend :: [a] -> ListAnd final a -> ListAnd final a
    prepend as bs = foldr Cons bs as
{-# NOINLINE buzHashList #-}

buzHashRepr :: BuzHash -> BuzHashRepr
buzHashRepr (BuzHash bhio) = unsafePerformIO (evaluate =<< BHIO.reprIO bhio)
{-# NOINLINE buzHashRepr #-}

buzTable :: Word8 -> Word32
buzTable idx = unsafeDupablePerformIO (BHIO.buzTableIO idx)
{-# INLINE buzTable #-}
