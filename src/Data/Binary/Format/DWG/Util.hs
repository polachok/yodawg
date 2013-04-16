module Data.Binary.Format.DWG.Util where
import Control.Monad (replicateM)
import Data.Word (Word8)
import Data.Binary

-- | repeat while condition doesn't hold and once more
repeatUntil :: Monad m => (a -> Bool) -> m a -> m [a]
repeatUntil pred act =
    let loop xs = do
        x <- act
        if pred x
           then return (x:xs)
           else loop (x:xs)
    in
        loop []

-- | ensure right sentinel value, calls fail if wrong
--   string to indicate where error comes from
ensureSentinel :: [Word8] -> String -> Get ()
ensureSentinel sentinel msg = do
    sent <- replicateM 16 get :: Get [Word8]
    if sent == sentinel
         then return ()
         else fail $ "bad sentinel in " ++ msg
