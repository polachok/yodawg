module Data.Binary.Format.DWG.ObjectMap (ObjectMap) where
import Control.Applicative

import Data.Binary
import Data.Binary.Get (runGet)
import qualified Data.Binary.Bits.Get as Bits

import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.Util
import Data.Binary.Format.DWG.Bitcoded
import qualified Data.Binary.Format.DWG.Bitcoded as Bits

type Section = (Int, [(DWG_MC, DWG_MC)])
newtype ObjectMap = ObjectMap [(DWG_H, DWG_MC)] deriving (Show)

handlePlusOffset :: DWG_H -> DWG_MC -> DWG_H
handlePlusOffset h o =
    case h of
        (DWG_H code handle) -> DWG_H code (handle + fromIntegral o)

decodeSection :: Bits.BitGet Section
decodeSection = do
        size <- Bits.getWord16be 16
        -- "each section is cut off at maximim length of 2032"
        let size' = if size > 2032 then 2032 else fromIntegral size
        bs <- Bits.getLazyByteString (size' - 2) -- because crc
        let s = flip runGet bs $ Bits.runBitGet $ many $ do
                -- handle offset from previous handle
                ho <- Bits.get
                -- location offset
                lo <- Bits.get
                return (ho, lo) 
        crc <- Bits.getWord16be 16
        return (fromIntegral size, s)

instance Binary ObjectMap where
    put = undefined 
    get = do
        let lastHandle = DWG_H 0 0
            lastLoc = 0
        ss <- concatMap snd <$> (Bits.runBitGet $ repeatUntil ((== 2) . fst) $ decodeSection)
        return $ ObjectMap $ scanl (\(h,l) (ho, lo) -> (handlePlusOffset h ho, l + lo)) (lastHandle, lastLoc) ss
