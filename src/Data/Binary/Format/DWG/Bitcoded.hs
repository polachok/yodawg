{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Bitcoded where
import Control.Applicative

import Data.Binary.Bits
import Data.Binary.Bits.Get
import qualified Data.Binary.Get as Binary
import Data.Binary.IEEE754

import Types

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
