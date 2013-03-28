{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HeaderVariables where
import Control.Applicative
import Data.Binary hiding (get)
import qualified Data.Binary as Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as State
import Types
import TH
import Bitcoded as Bits

import Debug.Trace

mkVariableAdt "Variable" "R15"

data Variables = Variables {
                  v_size :: !DWG_RL
                , v_vars :: [Variable]
                 } deriving (Show)

instance Binary Variables where
   put = undefined
   get = do
      let begginningSentinel = [0xcf, 0x7b, 0x1f, 0x23,
                                0xfd, 0xde, 0x38, 0xa9,
                                0x5f, 0x7c, 0x68, 0xb8,
                                0x4e, 0x6d, 0x33, 0x5f]
      sent <- mapM (const (Binary.get :: Get Word8)) [1..16]
      if sent == begginningSentinel
         then return ()
         else fail "wrong sentinel"
      size <- Binary.get :: Get DWG_RL
      xs <- Bits.runBitGet $! sequence (take 80 parseVariablesR15)
      return (Variables size xs)
