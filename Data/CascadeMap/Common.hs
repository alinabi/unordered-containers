{-# LANGUAGE CPP, BangPatterns #-}

module Data.CascadeMap.Common 
    ( Map
    , lookup
    ) where

import Data.Bits ( (.&.), shiftR )
import Data.Hashable
import qualified Data.FullList.Lazy as FL
import Prelude hiding ( lookup )

type Path = Int

data Octo k v = Octo {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)
                     {-# UNPACK #-} !(Bucket k v)

set :: Octo k v -> Path -> Bucket k v -> Octo k v
set (Octo a b c d e f g h) n x = case n .&. 0x7 of
    0x0 -> Octo x b c d e f g h
    0x1 -> Octo a x c d e f g h
    0x2 -> Octo a b x d e f g h
    0x3 -> Octo a b c x e f g h
    0x4 -> Octo a b c d x f g h
    0x5 -> Octo a b c d e x g h
    0x6 -> Octo a b c d e f x h
    _   -> Octo a b c x e f g x
{-# INLINE set #-}

get :: Octo k v -> Path -> Bucket k v
get (Octo a b c d e f g h) n = case n .&. 0x7 of
    0x0 -> a
    0x1 -> b
    0x2 -> c
    0x3 -> d
    0x4 -> e
    0x5 -> f
    0x6 -> g
    _   -> h
{-# INLINE get #-}
 
data Bucket k v = Bucket {-# UNPACK #-} !Path !(FL.List k v)

lookupB :: Eq k => Path -> k -> Bucket k v -> Maybe v
lookupB !h k (Bucket h' fl)
    | h' /= h   = Nothing
    | otherwise = FL.lookupL k fl
{-# INLINE lookupB #-}


data Map k v = Empty
             | Tip {-# UNPACK #-} !(Octo k v)
             | Bin {-# UNPACK #-} !(Octo k v) !(Map k v) !(Map k v)

empty :: Map k v
empty = Empty
{-# INLINE empty #-}

null :: Map k v -> Bool
null Empty = True
null _     = False
{-# INLINE null #-}

lookup :: (Eq k, Hashable k) => k -> Map k v -> Maybe v
lookup k m = go h m where
    !h = hash k
    go !p Empty = Nothing
    go !p (Tip o) = lookupB h k (o `get` p)
    go !p (Bin o l r) = case lookupB h k (o `get` p) of
        Nothing -> case fork p of
            0 -> go (advance p) l
            _ -> go (advance p) r  
        just -> just
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

delete :: (Eq k, Hashable k) => k -> Map k v -> Map k v
delete k m =
    let !h = hash k
        go !_ Empty = Nothing
        go !p (Tip o) = Tip o' where 
            b  = deleteB h k (o `get` p)
            o' = set o p b  
        go !p (Bin o l r)
    in
        case go h m of
            Nothing -> m
            Just m' -> m'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif


-- 
-- utility functions
--

advance :: Path -> Path
advance !p = p `shiftR` 4
{-# INLINE advance #-}

fork :: Path -> Int
fork !p = p .&. 0x8
{-# INLINE fork #-}
