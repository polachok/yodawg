module HeaderVariables where
import Data.Binary
import Types

data Variables = Variables {
                  v_size :: DWG_RL
                 }
 deriving (Show)

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
      return (Variables size)
