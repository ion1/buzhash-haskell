module Table (tests) where

import Crypto.Classes.Exceptions
import Crypto.Hash.CryptoAPI (SHA512)
import Crypto.Random.DRBG (HashDRBGWith)
import Crypto.Util (for)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vec
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Data.BuzHash (buzTable)

tests :: TestTree
tests =
  testGroup "Data.BuzHash.buzTable"
    [ testCase "buzTable is correct"
               (evalTable buzTable @?= evalTable buzTableRef)
    ]
  where
    evalTable f = map f [minBound..maxBound]

buzTableRef :: Word8 -> Word32
buzTableRef idx = buzTableRefVec ! fromIntegral idx

type RandomGenerator = HashDRBGWith SHA512

-- The key is all zeros.
entropy :: ByteString
entropy = BS.replicate (genSeedLength `for` generator) 0

generator :: RandomGenerator
generator = newGen entropy

buzTableRefVec :: Vector Word32
buzTableRefVec = Vec.fromList w32s
  where
    w32s = go (map fromIntegral (BS.unpack bs))
      where
        go (a:b:c:d:w8s) = w32 `seq` (w32 : go w8s)
          where w32 = (a `shiftL` 24)
                  .|. (b `shiftL` 16)
                  .|. (c `shiftL`  8)
                  .|. d
        go _ = []

    -- 256 slots, 32 bits = 4 bytes each.
    (bs, _) = genBytes (256 * 4) generator
