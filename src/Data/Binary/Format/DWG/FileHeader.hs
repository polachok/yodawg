{-# LANGUAGE OverloadedStrings #-}
module Data.Binary.Format.DWG.FileHeader where
import Control.Monad (when, replicateM)
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Char8 (ByteString, pack)
import Data.Word
import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.Util

data Version = R13 | R14 | R15 deriving (Show)
type Seeker = Word32
data Record = Record { r_serial :: !DWG_RB
                     , r_seeker :: !DWG_RL
                     , r_size   :: !DWG_RL }
                  deriving (Show)

data FileHeader = FileHeader {
                      version   :: !Version
                    , imageData :: !Seeker
                    , codepage  :: !DWG_RS
                    , n_recsets :: !DWG_RL
                    , records   :: ![Record]
                    , crc       :: !DWG_RS }
                  deriving (Show)

instance Binary Version where
   put = undefined
   get = do
      v <- getByteString 6
      case v of
         "AC1012" -> return R13
         "AC1014" -> return R14
         "AC1015" -> return R15
         _ -> fail "uknown version"

instance Binary Record where
   put = undefined
   get = do
      recnum <- getWord8 :: Get DWG_RB
      seeker <- get :: Get DWG_RL
      size   <- get :: Get DWG_RL
      return (Record recnum seeker size)

instance Binary FileHeader where
   put = undefined
   get = do
      let sentinel = [0x95, 0xa0, 0x4e, 0x28,
                      0x99, 0x82, 0x1a, 0xe5,
                      0x5e, 0x41, 0xe0, 0x5f,
                      0x9d, 0x3a, 0x4d, 0x00]
      v <- get :: Get Version
      skip 7 -- meaning unknown
      s <- getWord32le :: Get Seeker
      skip 2 -- object free space & template
      cp <- get :: Get DWG_RS
      nr@(DWG_RL n)  <- get :: Get DWG_RL
      rs <- replicateM (fromIntegral n) get :: Get [Record]
      crc <- get :: Get DWG_RS
      ensureSentinel sentinel "FileHeader"
      return (FileHeader v s cp nr rs crc) 
