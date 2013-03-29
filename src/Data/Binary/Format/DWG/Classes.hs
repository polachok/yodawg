module Data.Binary.Format.DWG.Classes where
import Control.Applicative
import Control.Monad
import Data.Binary hiding (get)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Monoid ((<>))
import Prelude hiding (catch)

import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.Bitcoded
import Debug.Trace

data Classes = Classes {
                  c_len :: !DWG_RL
                , c_classes :: [Class]
                , c_crc :: !DWG_RS
               } deriving (Show)

instance Binary Classes where
    put = undefined
    get = do
        let sentinel = [0x8D, 0xA1, 0xC4, 0xB8,
                        0xC4, 0xA9, 0xF8, 0xC5,
                        0xC0, 0xDC, 0xF4, 0x5F,
                        0xE7, 0xCF, 0xB6, 0x8A]
        let eSentinel = [0x72,0x5E,0x3B,0x47,
                         0x3B,0x56,0x07,0x3A,
                         0x3F,0x23,0x0B,0xA0,
                         0x18,0x30,0x49,0x75] 

        let manyClasses xs = do b <- Bits.isEmpty
                                if b
                                then return xs
                                else (get :: Bits.BitGet Class) >>=
                                         \x -> manyClasses (x:xs)
        sent <- mapM (const (Binary.get :: Get Word8)) [1..16]
        if sent == sentinel
           then return ()
           else fail "wrong sentinel"
        len <- Binary.get :: Binary.Get DWG_RL
        str <- Binary.getLazyByteString (fromIntegral len)
        let xs = Binary.runGet (Bits.runBitGet $ manyClasses []) str
        crc <- Binary.get :: Binary.Get DWG_RS
        sent <- mapM (const (Binary.get :: Get Word8)) [1..16]
        if sent == eSentinel
           then return ()
           else fail "wrong sentinel"
        return $! Classes len xs crc
