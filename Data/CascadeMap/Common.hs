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

emptyO :: Octo k v
emptyO = Octo b b b b b b b b where
    b = Bucket 0 FL.Nil
{-# NOINLINE emptyO #-}

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

singletonB :: Path -> k -> v -> Bucket k v
singletonB h k v = Bucket h . FL.Cons k v $ FL.Nil
{-# INLINE singletonB #-}

lookupB :: Eq k => Path -> k -> Bucket k v -> Maybe v
lookupB !h k (Bucket h' l)
    | h' /= h   = Nothing
    | otherwise = FL.lookupL k l 
{-# INLINE lookupB #-}

deleteB :: Eq k => Path -> k -> Bucket k v -> Maybe (Bucket k v)
deleteB !h k b@(Bucket h' l)
    | h' /= h   = Nothing
    | otherwise = Just . Bucket h' . FL.deleteL k $ l
{-# INLINE deleteB #-}

insertB :: Eq k => Path -> k -> v -> Bucket k v -> Maybe (Bucket k v)
insertB h k v b@(Bucket h' l)
    | nullL l || h == h' = Just . Bucket h . FL.insertL k v $ l
    | otherwise          = Nothing
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
    go !p t@(Tip o) = 
        case deleteB h k $ o `get` p of
            Just b -> Tip $ set o p b
            _      -> t
    go !p (Bin o l r) = 
        case deleteB h k $ o `get` p of
            Just b -> Bin (set o p b) l r
            _      -> case fork p of
                          0 -> Bin o l' r
                          _ -> Bin o l r' 
        where
            l' = go (advance p) l
            r' = go (advance p) r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

insert :: (Eq k, Hashable k) => k -> v -> Map k v -> Map k v
insert k v m = go h m where
    !h = hash k
    go !p Empty = go p (Tip emptyO)
    go !p t@(Tip o) = 
        case insertB h k v $ o `get` p of
            Just b -> Tip $ set o p b
            _      -> case fork p of
                          0 -> Bin o x Empty
                          _ -> Bin o Empty x
        where
            x = go (advance p) Empty
    go !p (Bin o l r) = 
        case insertB h k v $ o `get` p of
            Just b -> Bin (set o p b) l r
            _      -> case fork p of
                          0 -> Bin o l' r
                          _ -> Bin o l r'
        where
            l' = go (advance p) l
            r' = go (advance p) r
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

nullL :: FL.List k v -> Bool
nullL FL.Nil = True
nullL_       = False
{-# INLINE nullL #-}
