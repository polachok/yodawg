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
    get = DWG_RD <$> Binary.runGet getFloat64le <$> getLazyByteString' 8

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

shiftRight :: [Word8] -> Int
shiftRight xs =
    let
      x = Binary.runGet (Binary.getWord32be) $ Binary.runPut (runBitPut $ do
        replicateM_ (32 - 7 * length xs) $ putBool False
        mapM_ (putWord8 7) $ reverse xs)
      signBit = 7 * length xs - 1
    in
      if testBit x signBit
      then 0 - (fromIntegral (clearBit x signBit))
      else fromIntegral x

instance Bitcoded DWG_MC where
    get = do xs <- readUntilHighZero []
             return $! DWG_MC $ fromIntegral $ shiftRight xs
           where readUntilHighZero xs = do
                    x <- fromIntegral <$> getWord8 8
                    if not (testBit x 7)
                    then return (x:xs)
                    else readUntilHighZero (x:xs)
