module Data.BuzHash.IO
  ( ChunkSplit (..)
  , BuzHashIO
  , BuzHashRepr (..)
  , newIO
  , cloneIO
  , processIO
  , reprIO
  , buzTableIO
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.IORef
import Foreign
import Foreign.C

import Data.BuzHash.Internal.ChunkSplit
import Data.BuzHash.Internal.Prim

newtype BuzHashIO = BuzHashIO (ForeignPtr C'buzhash_t)

data BuzHashRepr
  = BuzHashRepr { bhrWindowSize   :: !Integer  -- ^ window size, divided by 32
                , bhrChunkMinSize :: !Integer  -- ^ minimum chunk size
                , bhrChunkMaxSize :: !Integer  -- ^ maximum chunk size
                , bhrMaskBits     :: !Integer  -- ^ number of mask bits, max 32

                , bhrRingBuf        :: !ByteString
                , bhrHash           :: !Word32
                , bhrChunkTotalSize :: !Integer
                }
  deriving (Eq, Show)

newIO :: Integer  -- ^ window size, divided by 32
      -> Integer  -- ^ minimum chunk size
      -> Integer  -- ^ maximum chunk size
      -> Integer  -- ^ number of mask bits, max 32
      -> IO BuzHashIO
newIO windowSize chunkMinSize chunkMaxSize maskBits = do
  cbh <- throwErrnoIfNullRetry "buzhash_new" 
       $ c'buzhash_new (fromIntegral windowSize) (fromIntegral chunkMinSize)
                       (fromIntegral chunkMaxSize) (fromIntegral maskBits)

  BuzHashIO <$> newForeignPtr p'buzhash_free cbh

cloneIO :: BuzHashIO -> IO BuzHashIO
cloneIO (BuzHashIO bhFPOrig) = do
  bhP <- withForeignPtr bhFPOrig $ \bhPOrig ->
    throwErrnoIfNullRetry "buzhash_clone" (c'buzhash_clone bhPOrig)

  BuzHashIO <$> newForeignPtr p'buzhash_free bhP

processIO :: BuzHashIO -> ByteString -> IO [ChunkSplit ByteString]
processIO (BuzHashIO bhFP) bs = do
  resultR <- newIORef dlistEmpty
  callback <- mk'split_ptr (\n -> modifyIORef' resultR (dlistSnoc n))

  withForeignPtr bhFP $ \bhP -> do
    BS.unsafeUseAsCStringLen bs $ \(buf, bufSize) -> do
      c'buzhash_process bhP (castBuf buf) (fromIntegral bufSize) callback

  splits <- map fromIntegral . dlistToList <$> readIORef resultR

  return (splitByteString bs splits)

  where
    castBuf :: Ptr CChar -> Ptr Word8
    castBuf = castPtr

    dlistEmpty = id
    dlistSnoc x f = f . (x:)
    dlistToList f = f []

reprIO :: BuzHashIO -> IO BuzHashRepr
reprIO (BuzHashIO bhFP) =
  withForeignPtr bhFP $ \bhP -> do
    bhC <- peek bhP
    let rbP = c'buzhash_t'ringbuf bhC
    rb <- peekRingBuf rbP
    return $! BuzHashRepr{ bhrWindowSize     = fromIntegral (c'buzhash_t'window_size    bhC)
                         , bhrChunkMinSize   = fromIntegral (c'buzhash_t'chunk_min_size bhC)
                         , bhrChunkMaxSize   = fromIntegral (c'buzhash_t'chunk_max_size bhC)
                         , bhrMaskBits       = fromIntegral (c'buzhash_t'mask_bits      bhC)
                         , bhrRingBuf        = rb
                         , bhrHash           = c'buzhash_t'hash bhC
                         , bhrChunkTotalSize = fromIntegral (c'buzhash_t'chunk_total_size bhC)
                         }

peekRingBuf :: Ptr C'ringbuf_t -> IO ByteString
peekRingBuf rbP = do
  rbC <- peek rbP
  bufOrig <- BS.packCStringLen ( (castBuf (p'ringbuf_t'buf rbP))
                               , (fromIntegral (c'ringbuf_t'size rbC))
                               )
  let (bufR, bufL) = BS.splitAt (fromIntegral (c'ringbuf_t'idx rbC)) bufOrig
  return $! (bufL `BS.append` bufR)

  where
    castBuf :: Ptr Word8 -> Ptr CChar
    castBuf = castPtr

buzTableIO :: Word8 -> IO Word32
buzTableIO idx = peekElemOff c'buztable (fromIntegral idx)
{-# INLINE buzTableIO #-}
