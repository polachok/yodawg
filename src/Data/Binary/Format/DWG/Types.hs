{-# LANGUAGE TypeSynonymInstances #-}
module Types where
import Data.Binary
import Data.Bits
import Data.Text (Text)

type DWG_B   = Word8 -- bit
type DWG_BB  = Word8 -- 2 bit
type DWG_3B  = Word8 -- (1-3 bits)
type DWG_BS  = Word16 -- (16 bits)
type DWG_BL  = Word32 -- (32 bits)
type DWG_BLL = Word64 -- (64 bits)
type DWG_BD  = Double -- bitdouble
type DWG_2BD = (DWG_BD, DWG_BD) -- 2d point
type DWG_3BD = (DWG_BD, DWG_BD, DWG_BD) -- 3d point
type DWG_RB  = Word8 -- raw byte
type DWG_RC  = Char -- raw char
type DWG_RS  = Word16 -- raw short
type DWG_RD  = Double -- raw double
type DWG_RL  = Word32 -- raw long
type DWG_2RD = (Double, Double) -- 2 raw doubles
type DWG_3RD = (Double, Double, Double) -- 3 raw doubles
type DWG_MC  = Char -- modular char 
type DWG_MS  = Int -- modular short
--newtype DWG_H   =  -- handle reference
type DWG_T   = Text -- bitshort length, followed by a string
type DWG_TU  = Text -- bitshort character length, followed
                    -- by Unicode string, 2 bytes per character
--newtype DWG_TV = DWG_T for 2004 and earlier else TU
--newtype DWG_X   = -- special form
--newtype DWG_U   = -- unknown
type DWG_SN  = Int -- 16 byte sentinel
--newtype DWG_BE  = -- bit extrusion
--newtype DWG_DD  = -- bit double with default
--newtype DWG_BT  = -- bit thickness
--newtype DWG_3DD = (DWG_DD, DWG_DD, DWG_DD)
--newtype DWG_CMC = CmColor value
--newtype DWG_TC = true color, same as CMC
--newtype DWG_OT = Object
