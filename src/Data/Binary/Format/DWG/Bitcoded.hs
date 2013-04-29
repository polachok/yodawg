{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Binary.Format.DWG.Bitcoded where
import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Word (Word8,Word16,Word32)
import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import Data.Binary.IEEE754

import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.Util

class Bitcoded a where
    get :: BitGet a

instance Bitcoded DWG_H where
    get = getHandle

instance Bitcoded DWG_BS where
    get = getBitShort

instance Bitcoded DWG_BD where
    get = getBitDouble

instance Bitcoded DWG_BL where
    get = getBitLong

instance Bitcoded DWG_T where
    get = getText

instance Bitcoded DWG_B where
    get = getBool

instance Bitcoded DWG_RD where
    get = DWG_RD <$> Binary.runGet getFloat64le <$> getLazyByteString 8

instance Bitcoded DWG_2RD where
    get = (,) <$> get <*> get

instance Bitcoded DWG_3BD where
    get = (,,) <$> get <*> get <*> get

instance Bitcoded DWG_RC where
    get = getWord8 8

instance Bitcoded DWG_CMC where
    get = DWG_CMC <$> get

instance Bitcoded DWG_WH where
    get = getWideHandle

instance Bitcoded DWG_BSH where
    get = getBitShortMaybeHandle

instance Bitcoded Class where
    get = Class <$> get <*> get <*> get <*> get <*> get <*> get <*> get

{--
shiftRight :: [Word8] -> Int
shiftRight xs =
    let
      bitsize = bitSize (xs !! 0) - 1 -- first bit dropped
      x = Binary.runGet (Binary.getWord32be) $ Binary.runPut (runBitPut $ do
            replicateM_ (32 - bitsize * length xs) $ putBool False
            mapM_ (putWord8 bitsize) $ reverse xs)
      signBit = bitsize * length xs - 1
    in
      if testBit x signBit
      then 0 - (fromIntegral (clearBit x signBit))
      else fromIntegral x
--}

shiftRight16 :: (Either [Word8] [Word16]) -> Int
shiftRight16 = \e -> case e of
           Right ws -> t (g (bitsize ws) ws) (signBit ws)
           Left ws -> t (g (bitsize ws) ws) (signBit ws)
    where
      bitsize ws = bitSize (head ws) - 1 -- first bit dropped
      signBit ws = (bitsize ws) * (length ws) - 1
      g bs xs = Binary.runGet (Binary.getWord32be) $ Binary.runPut (runBitPut $ do
              replicateM_ (32 - bs * length xs) $ putBool False
              mapM_ (putWord16be bs) $ reverse (map fromIntegral xs))
      t x sb = if testBit x sb
               then 0 - (fromIntegral (clearBit x sb))
               else fromIntegral x

instance Bitcoded DWG_MC where
    get = do xs <- repeatUntil (flip testBit 7) (getWord8 8)
             return $! DWG_MC $ fromIntegral $ shiftRight16 (Left xs)

instance Bitcoded DWG_MS where
    get = do xs <- repeatUntil (flip testBit 15) (getWord16be 16)
             return $! DWG_MS $ fromIntegral $ shiftRight16 (Right xs)

instance Bitcoded DWG_BT where
    -- R15+
    get = getBool >>= \b ->
             if b
             then return $ DWG_BT 0.0
             else DWG_BT <$> (\(DWG_BD f) -> f) <$> getBitDouble
