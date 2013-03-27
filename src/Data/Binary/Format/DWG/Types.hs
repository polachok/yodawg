{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Types where
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.Binary.Bits.Get as Bits
import Control.Applicative
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Debug.Trace

type DWG_B   = Bool -- bit
type DWG_BB  = Word8 -- 2 bit
type DWG_3B  = Word8 -- (1-3 bits)
newtype DWG_BS  = DWG_BS Word16 -- (16 bits)
        deriving (Show,Enum,Real,Num,Ord,Eq,Integral)
newtype DWG_BL  = DWG_BL Word32 -- (32 bits)
        deriving (Show,Enum,Real,Num,Ord,Eq,Integral)
type DWG_BLL = Word64 -- (64 bits)
newtype DWG_BD  = DWG_BD Double deriving (Show) -- bitdouble
type DWG_2BD = (DWG_BD, DWG_BD) -- 2d point
type DWG_3BD = (DWG_BD, DWG_BD, DWG_BD) -- 3d point
type DWG_RB  = Word8 -- raw byte
type DWG_RC  = Char -- raw char
newtype DWG_RS  = DWG_RS Word16 deriving (Show) -- raw short
type DWG_RD  = Double -- raw double
newtype DWG_RL  = DWG_RL Word32 deriving (Show) -- raw long
type DWG_2RD = (Double, Double) -- 2 raw doubles
type DWG_3RD = (Double, Double, Double) -- 3 raw doubles
type DWG_MC  = Char -- modular char 
type DWG_MS  = Int -- modular short
newtype DWG_H  = DWG_H Word16 deriving (Show) -- handle reference
newtype DWG_T  = DWG_T ByteString deriving (Show) -- bitshort length, followed by a string
type DWG_TU  = ByteString -- bitshort character length, followed
                    -- by Unicode string, 2 bytes per character
type DWG_TV = DWG_T -- for 2004 and earlier else TU
--newtype DWG_X   = -- special form
--newtype DWG_U   = -- unknown
type DWG_SN  = Int -- 16 byte sentinel
--newtype DWG_BE  = -- bit extrusion
--newtype DWG_DD  = -- bit double with default
--newtype DWG_BT  = -- bit thickness
--newtype DWG_3DD = (DWG_DD, DWG_DD, DWG_DD)
data DWG_CMC = DWG_CMC DWG_BS DWG_BL DWG_RC deriving (Show)
--newtype DWG_TC = true color, same as CMC
--newtype DWG_OT = Object

instance Binary DWG_RS where
    put = undefined
    get = DWG_RS <$> getWord16le

instance Binary DWG_RL where
    put = undefined
    get = DWG_RL <$> getWord32le

instance Binary DWG_BD where
    put = undefined
    get = Bits.runBitGet getBitDouble

instance Binary DWG_BS where
    put = undefined
    get = Bits.runBitGet getBitShort

instance Binary DWG_T where
    put = undefined
    get = Bits.runBitGet getText

instance Binary DWG_H where
    put = undefined
    get = Bits.runBitGet getHandle

getHandle :: Bits.BitGet DWG_H
getHandle = do
        code <- Bits.getWord8 4
        counter <- Bits.getWord8 4
        if counter /= 0
           then DWG_H <$> runGet getWord16le <$> getLazyByteString' (fromIntegral counter)
           else return $ DWG_H 0

getBitShort :: Bits.BitGet DWG_BS
getBitShort = do
        i <- Bits.getWord8 2
        d <- case i of
                     0 -> runGet getWord16le <$> getLazyByteString' 2
                     1 -> fromIntegral <$> runGet getWord8 <$> getLazyByteString' 1
                     2 -> return 0
                     3 -> return 256
        return $! DWG_BS d

getBitDouble :: Bits.BitGet DWG_BD
getBitDouble = do
        i <- Bits.getWord8 2
        d <- case i of
                    0 -> runGet getFloat64le <$> getLazyByteString' 8
                    1 -> return 1.0
                    2 -> return 0.0
                    _ -> fail "bad DWG_BD"
        return $! DWG_BD d

getBitLong :: Bits.BitGet DWG_BL
getBitLong = do
        i <- Bits.getWord8 2
        l <- case i of
                0 -> runGet getWord32le <$> getLazyByteString' 4
                1 -> fromIntegral <$> runGet getWord8 <$> getLazyByteString' 1
                2 -> return 0
                4 -> fail "bad DWG_BL"
        return $! DWG_BL l

getText :: Bits.BitGet DWG_T
getText = do
    len <- getBitShort
    t <- Bits.getByteString (fromIntegral len)
    return $! DWG_T t

getLazyByteString' :: Int -> Bits.BitGet Lazy.ByteString
getLazyByteString' n = Lazy.fromChunks . flip (:) [] <$> Bits.getByteString n
