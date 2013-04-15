module Data.Binary.Format.DWG.Util where

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


