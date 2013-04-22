{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Data.Binary.Format.DWG.Types where
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
type DWG_RC  = Word8 -- raw char
newtype DWG_RS  = DWG_RS Word16
        deriving (Show,Eq,Enum,Real,Num,Ord,Integral) -- raw short
newtype DWG_RD  = DWG_RD Double deriving (Show) -- raw double
newtype DWG_RL  = DWG_RL Word32
        deriving (Show,Eq,Enum,Real,Num,Ord,Integral) -- raw long
type DWG_2RD = (DWG_RD, DWG_RD) -- 2 raw doubles
type DWG_3RD = (Double, Double, Double) -- 3 raw doubles
newtype DWG_MC = DWG_MC Int -- modular char 
        deriving (Show,Eq,Enum,Real,Num,Ord,Integral)
newtype DWG_MS = DWG_MS Integer -- modular short
        deriving (Show,Eq,Enum,Real,Num,Ord,Integral)
data DWG_BSH = DWG_BSH DWG_BS (Maybe DWG_H) deriving (Show) -- CEPSNTYPE 
data DWG_H  = DWG_H !Word8 !Word16 deriving (Show) -- handle reference
data DWG_WH  = DWG_WH !Word8 !Word16 deriving (Show) -- wide handle reference (HANDSEED)
newtype DWG_T  = DWG_T ByteString deriving (Show) -- bitshort length, followed by a string
type DWG_TU  = ByteString -- bitshort character length, followed
                    -- by Unicode string, 2 bytes per character
type DWG_TV = DWG_T -- for 2004 and earlier else TU
--newtype DWG_X   = -- special form
--newtype DWG_U   = -- unknown
type DWG_SN  = [Int] -- 16 byte sentinel
--newtype DWG_BE  = -- bit extrusion
--newtype DWG_DD  = -- bit double with default
newtype DWG_BT  = DWG_BT Double deriving (Show) -- bit thickness
--newtype DWG_3DD = (DWG_DD, DWG_DD, DWG_DD)
--data DWG_CMC = DWG_CMC DWG_BS DWG_BL DWG_RC deriving (Show)
newtype DWG_CMC = DWG_CMC DWG_BS deriving (Show)
--newtype DWG_TC = true color, same as CMC
--newtype DWG_OT = Object

data Class = Class !DWG_BS !DWG_BS !DWG_TV !DWG_TV !DWG_TV !DWG_B !DWG_BS
                deriving (Show)

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
        case counter of
            0 -> return $ DWG_H code 0
            1 -> Bits.getWord8 8 >>= \h -> return $ DWG_H code (fromIntegral h)
            2 -> DWG_H code <$> (runGet getWord16le <$> Bits.getLazyByteString (fromIntegral counter))
            _ -> error $ "Handle of length " ++ show counter

getBitShort :: Bits.BitGet DWG_BS
getBitShort = do
        i <- Bits.getWord8 2
        d <- case i of
                     0 -> runGet getWord16le <$> Bits.getLazyByteString 2
                     1 -> fromIntegral <$> runGet getWord8 <$> Bits.getLazyByteString 1
                     2 -> return 0
                     3 -> return 256
        return $! DWG_BS d

getBitShortMaybeHandle :: Bits.BitGet DWG_BSH
getBitShortMaybeHandle = do
        bs <- getBitShort
        case bs of
            3 -> DWG_BSH bs . Just <$> getHandle
            _ -> return $ DWG_BSH bs Nothing

getBitDouble :: Bits.BitGet DWG_BD
getBitDouble = do
        i <- Bits.getWord8 2
        d <- case i of
                    0 -> runGet getFloat64le <$> Bits.getLazyByteString 8
                    1 -> return 1.0
                    2 -> return 0.0
                    _ -> fail "bad DWG_BD"
        return $! DWG_BD d

getBitLong :: Bits.BitGet DWG_BL
getBitLong = do
        i <- Bits.getWord8 2
        l <- case i of
                0 -> runGet getWord32le <$> Bits.getLazyByteString 4
                1 -> fromIntegral <$> runGet getWord8 <$> Bits.getLazyByteString 1
                2 -> return 0
                4 -> fail "bad DWG_BL"
        return $! DWG_BL l

getText :: Bits.BitGet DWG_T
getText = do
    len <- getBitShort
    t <- Bits.getByteString (fromIntegral len)
    return $! DWG_T t

getWideHandle :: Bits.BitGet DWG_WH
getWideHandle = do
    counter <- Bits.getWord8 8
    let code = 0
    case counter of
            0 -> return $ DWG_WH code 0
            1 -> Bits.getWord8 8 >>= \h -> return $ DWG_WH code (fromIntegral h)
            2 -> DWG_WH code <$> runGet getWord16le <$> Bits.getLazyByteString (fromIntegral counter)
            _ -> error $ "Wide handle of length " ++ show counter
