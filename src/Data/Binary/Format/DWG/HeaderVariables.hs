{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.Binary.Format.DWG.HeaderVariables where
import Control.Applicative
import Data.Binary hiding (get)
import qualified Data.Binary as Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as State
import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.TH
import Data.Binary.Format.DWG.Util (ensureSentinel)
import Data.Binary.Format.DWG.Bitcoded as Bits

import Debug.Trace

mkVariableAdt "Variable" "R15"

data Variables = Variables {
                  v_size :: !DWG_RL
                , v_vars :: [Variable]
                , v_crc  :: !DWG_RS
                 } deriving (Show)

instance Binary Variables where
   put = undefined
   get = do
      let beginningSentinel = [0xcf, 0x7b, 0x1f, 0x23,
                                0xfd, 0xde, 0x38, 0xa9,
                                0x5f, 0x7c, 0x68, 0xb8,
                                0x4e, 0x6d, 0x33, 0x5f]

          endingSentinel = [0x30, 0x84, 0xE0, 0xDC,
                            0x02, 0x21, 0xC7, 0x56,
                            0xA0, 0x83, 0x97, 0x47,
                            0xB1,0x92,0xCC,0xA0]

      ensureSentinel beginningSentinel "HeaderVariables, beginning"
      size <- Binary.get :: Get DWG_RL
      xs <- Bits.runBitGet $! sequence parseVariablesR15
      crc <- Binary.get :: Get DWG_RS
      ensureSentinel endingSentinel "HeaderVariables, ending"
      return (Variables size xs crc)
