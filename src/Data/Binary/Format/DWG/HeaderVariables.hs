{-# LANGUAGE GADTs, ExistentialQuantification #-}
module HeaderVariables where
import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy.Char8
import qualified Data.Binary.Bits.Get as Bits
import Data.Text (Text)
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as State
import Types

data Variable = UNKNOWN_BD !DWG_BD | UNKNOWN_TV !DWG_TV
              | UNKNOWN_BL !DWG_BL | VIEWPORT !DWG_H
              | DIMASO !DWG_B | DIMSHO !DWG_B | DIMSAV !DWG_B
              | PLINEGEN !DWG_B | ORTHOMODE !DWG_B
              | REGENMODE !DWG_B | FILLMODE !DWG_B
              | QTEXTMODE !DWG_B | PSLTSCALE !DWG_B
              | LIMCHECK !DWG_B
      deriving (Show)

data Ctor a = forall a. Binary a => Ctor (a -> Variable)

varsR15 = [Ctor UNKNOWN_BD, Ctor UNKNOWN_BD, Ctor UNKNOWN_BD, Ctor UNKNOWN_BD] --,
      {--     Ctor UNKNOWN_TV, Ctor UNKNOWN_TV, Ctor UNKNOWN_TV, Ctor UNKNOWN_TV,
           Ctor UNKNOWN_BL, Ctor UNKNOWN_BL, Ctor VIEWPORT, Ctor DIMASO,
           Ctor DIMSHO, Ctor PLINEGEN, Ctor ORTHOMODE, Ctor REGENMODE,
           Ctor FILLMODE, Ctor QTEXTMODE, Ctor PSLTSCALE, Ctor LIMCHECK] -- more to come
--}
data Variables = Variables {
                  v_size :: !DWG_RL
                , v_vars :: ![Variable]
                 } deriving (Show)

instance Binary Variables where
   put = undefined
   get = do
      let begginningSentinel = [0xcf, 0x7b, 0x1f, 0x23,
                                0xfd, 0xde, 0x38, 0xa9,
                                0x5f, 0x7c, 0x68, 0xb8,
                                0x4e, 0x6d, 0x33, 0x5f]
      sent <- mapM (const (get :: Get Word8)) [1..16]
      if sent == begginningSentinel
         then return ()
         else fail "wrong sentinel"
      size <- get :: Get DWG_RL
      vs <- Bits.runBitGet $ do
         let go :: Ctor a -> State.StateT ByteString Bits.BitGet Variable
             go (Ctor f) = do
             bs <- State.get
             case runGetOrFail get bs of
                  Left (_, _, e) -> fail e
                  Right (bs', _, a) -> State.put bs' >> return (f a)
         let (DWG_RL s) = size
         bs <- Bits.getLazyByteString (fromIntegral s)
         flip State.evalStateT bs $ mapM go varsR15
      return (Variables size vs)
