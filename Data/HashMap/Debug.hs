module Data.HashMap.Debug where

import qualified Data.FullList.Lazy as FL
import Data.HashMap.Common

bucketList :: HashMap k v -> [(Hash, FL.FullList k v)]
bucketList = go [] where
    go z (Bin _ _ l r) = go (go z l) r
    go z (Tip h l) = (h, l) : z
    go z Nil = z 
