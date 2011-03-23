{-# LANGUAGE CPP, BangPatterns #-}

module Data.CascadeMap.Common 
    ( Map
    , lookup
    , insert
    , delete
    ) where

import Data.Bits ( (.&.), shiftR, bitSize )
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
lookupB !h k (Bucket h' l) = go h' l where
    go x | x /= h    = Nothing
         | otherwise = FL.lookupL k
{-# INLINE lookupB #-}

deleteB :: Eq k => Path -> k -> Bucket k v -> Either (Bucket k v) (Bucket k v)
deleteB !h k b@(Bucket h' l) = go h' l where
    go x | x /= h    = Left b
         | otherwise = Right . Bucket h' . FL.deleteL k
{-# INLINE deleteB #-}

insertB :: Eq k => Path -> k -> v -> Bucket k v -> Either (Bucket k v) (Bucket k v)
insertB h k v b@(Bucket h' l) = go h' l where
    go x | x /= h    = Left b
         | otherwise = Right . Bucket h' . FL.insertL k v 
{-# INLINE insertB #-}


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
delete k m = go h m where
    !h = hash k
    go !_ Empty = Empty
    go !p m@(Tip o) = case deleteB h k $ o `get` p of
        Left  _ -> m
        Right b -> Tip $ set o p b
    go !p (Bin o l r) = case deleteB h k $ o `get` p of
        Right b -> Bin (set o p b) l r
        Left  _ -> case fork p of
            0 -> let l' = go (advance p) l in Bin o l' r
            _ -> let r' = go (advance p) r in Bin o l r' 
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

insert :: (Eq k, Hashable k) => k -> v -> Map k v -> Map k v
insert k v m = go (bitSize h) h m where
    !h = hash k
    go 0 _ m = m
    go !_ !p Empty = Tip $ set emptyO p (singletonB h k v)
    go !n !h (Tip o) = case insertB h k v $ o `get` p of
        Right b -> Tip $ set o p b
        Left  _ -> let x = go (n-4) (advance p) Empty in
            case fork p of
                0 -> Bin o x Empty
                _ -> Bin o Empty x
    go !n !h (Bin o l r) = case insertB h k v $ o `get` p of
        Right b -> Bin (set o p b) l r
        Left  _ -> case fork p of
            0 -> let l' = go (n-4) (advance p) l in Bin o l' r
            _ -> let r' = go (n-4) (advance p) r in Bin o l r'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
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
