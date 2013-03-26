{-# LANGUAGE OverloadedStrings #-}
module HeaderVariables where
import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Char8 (ByteString)
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as State
import Types

import Debug.Trace

varsR15 = [("Unknown",  	BitDouble <$> getBitDouble),
           ("Unknown",  	BitDouble <$> getBitDouble),
           ("Unknown",  	BitDouble <$> getBitDouble),
           ("Unknown",  	BitDouble <$> getBitDouble),
           ("Unknown",  	BitText   <$> getText),
           ("Unknown",  	BitText   <$> getText),
           ("Unknown",  	BitText   <$> getText),
           ("Unknown",  	BitText   <$> getText),
           ("Unknown",  	BitLong   <$> getBitLong),
           ("Unknown",  	BitLong   <$> getBitLong),
           ("VIEWPORT", 	BitHandle <$> getHandle),
           ("DIMASO",   	Bit 	  <$> Bits.getBool),
           ("DIMSHO",   	Bit 	  <$> Bits.getBool),
           ("PLINEGEN", 	Bit 	  <$> Bits.getBool),
           ("ORTHOMODE",	Bit 	  <$> Bits.getBool),
           ("REGENMODE",	Bit 	  <$> Bits.getBool),
           ("FILLMODE", 	Bit 	  <$> Bits.getBool),
           ("QTEXTMODE",	Bit 	  <$> Bits.getBool),
           ("PSLTSCALE",	Bit 	  <$> Bits.getBool),
           ("LIMCHECK", 	Bit 	  <$> Bits.getBool),
           ("USRTIMER", 	Bit 	  <$> Bits.getBool),
           ("SKPOLY",   	Bit 	  <$> Bits.getBool),
           ("ANGDIR",   	Bit       <$> Bits.getBool),
           ("SPLFRAME", 	Bit       <$> Bits.getBool),
           ("MIRRTEXT", 	Bit       <$> Bits.getBool),
           ("WORLDVIEW",	Bit       <$> Bits.getBool),
           ("TILEMODE", 	Bit       <$> Bits.getBool),
           ("LIMCHECK",	    Bit       <$> Bits.getBool),
           ("VISRETAIN",	Bit       <$> Bits.getBool),
           ("DISPSILH",	    Bit       <$> Bits.getBool),
           ("PELLIPSE",	    Bit       <$> Bits.getBool),
           ("PROXYGRAPHICS",BitShort  <$> getBitShort),
           ("TREEDEPTH",	BitShort  <$> getBitShort),
           ("LUNITS",		BitShort  <$> getBitShort),
           ("LUPREC",		BitShort  <$> getBitShort),
           ("AUNITS",		BitShort  <$> getBitShort),
           ("AUPREC",		BitShort  <$> getBitShort),
           ("ATTMODE",		BitShort  <$> getBitShort),
           ("PDMODE",		BitShort  <$> getBitShort),
           ("USERI1",		BitShort  <$> getBitShort),
           ("USERI2",		BitShort  <$> getBitShort),
           ("USERI3",		BitShort  <$> getBitShort),
           ("USERI4",		BitShort  <$> getBitShort),
           ("USERI5",		BitShort  <$> getBitShort),
           ("SPLINESEGS",	BitShort  <$> getBitShort),
           ("SURFU",		BitShort  <$> getBitShort),
           ("SURFV",		BitShort  <$> getBitShort),
           ("SURFTYPE",	    BitShort  <$> getBitShort),
           ("SURFTAB1",	    BitShort  <$> getBitShort),
           ("SURFTAB2",	    BitShort  <$> getBitShort),
           ("SPLINETYPE",	BitShort  <$> getBitShort),
           ("SHADEDGE",		BitShort  <$> getBitShort),
           ("SHADEDIF",		BitShort  <$> getBitShort),
           ("UNITMODE",		BitShort  <$> getBitShort),
           ("MAXACTVP",		BitShort  <$> getBitShort),
           ("ISOLINES",		BitShort  <$> getBitShort),
           ("CMLJUST",	   	BitShort  <$> getBitShort),
           ("TEXTQLTY",	    BitShort  <$> getBitShort),
           ("LTSCALE",	    BitDouble <$> getBitDouble),
           ("TEXTSIZE",		BitDouble <$> getBitDouble),
           ("TRACEWID",		BitDouble <$> getBitDouble),
           ("SKETCHINC",	BitDouble <$> getBitDouble),
           ("FILLETRAD",	BitDouble <$> getBitDouble),
           ("THICKNESS",	BitDouble <$> getBitDouble),
           ("ANGBASE",	    BitDouble <$> getBitDouble),
           ("PDSIZE",	    BitDouble <$> getBitDouble),
           ("PLINEWID",	    BitDouble <$> getBitDouble),
           ("USERR1",	    BitDouble <$> getBitDouble),
           ("USERR2",	    BitDouble <$> getBitDouble),
           ("USERR3",	    BitDouble <$> getBitDouble),
           ("USERR4",	    BitDouble <$> getBitDouble),
           ("USERR5",	    BitDouble <$> getBitDouble),
           ("CHAMFERA",		BitDouble <$> getBitDouble),
           ("CHAMFERB",		BitDouble <$> getBitDouble),
           ("CHAMFERC",		BitDouble <$> getBitDouble),
           ("CHAMFERD",		BitDouble <$> getBitDouble),
           ("FACETRES",		BitDouble <$> getBitDouble),
           ("CMLSCALE",		BitDouble <$> getBitDouble),
           ("CELTSCALE",	BitDouble <$> getBitDouble),
           ("MENUNAME",		BitDouble <$> getBitDouble),
           ("TDCREATE",		BitLong	  <$> getBitLong),
           ("TDCREATE",		BitLong	  <$> getBitLong),
           ("TDUPDATE",		BitLong	  <$> getBitLong),
           ("TDUPDATE",		BitLong	  <$> getBitLong),
           ("TDINDWG",	    BitLong	  <$> getBitLong),
           ("TDINDWG",	    BitLong	  <$> getBitLong),
           ("TDUSRTIMER",	BitLong	  <$> getBitLong),
           ("TDUSRTIMER",	BitLong	  <$> getBitLong)]

data Value = BitDouble DWG_BD
           | BitShort  DWG_BS 
           | BitLong   DWG_BL
           | BitText   DWG_T
           | Bit       DWG_B
           | BitHandle DWG_H
            deriving (Show)

data Variables = Variables {
                  v_size :: !DWG_RL
                , v_vars :: Map.Map ByteString Value
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
      xs <- Bits.runBitGet $! mapM (\(s, f) -> f >>= \x -> return (s, x)) varsR15
      return (Variables size (Map.fromList xs))
