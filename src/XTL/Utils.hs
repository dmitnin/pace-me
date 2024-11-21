module XTL.Utils (safeHead, getLeft, getRight) where


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

getLeft :: Either a b -> a
getLeft (Left x) = x

getRight :: Either a b -> b
getRight (Right x) = x
