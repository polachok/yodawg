{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.Binary.Format.DWG.File where
import Data.Binary
import Data.Binary.Get
import Data.Binary.Format.DWG.Types
import Data.Binary.Format.DWG.FileHeader
import Data.Binary.Format.DWG.HeaderVariables
import Data.Binary.Format.DWG.Classes
import qualified Data.ByteString.Lazy.Char8 as BS

data File = File { 
                   header  :: FileHeader
                 , vars    :: Variables
                 , classes :: Classes } deriving (Show)

goto :: DWG_RL -> Get ()
goto absolute = do
    x <- bytesRead
    let (DWG_RL a) = absolute
    let s = fromIntegral $ fromIntegral a - x
    skip s

instance Binary File where
    put = undefined
    get = do
        h <- get :: Get FileHeader
        let r0 = r_seeker $ records h !! 0
        goto r0
        vs <- get :: Get Variables
        cs <- get :: Get Classes
        skip 0x200 -- padding
        return (File h vs cs)
