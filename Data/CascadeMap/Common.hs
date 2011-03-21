{-# LANGUAGE CPP, BangPatterns #-}

module Data.CascadeMap.Common 
    ( Map
    , lookup
    ) where

import Data.Bits ( (.&.), shiftR )
import Data.Hashable
import qualified Data.FullList.Lazy as FL
import Prelude hiding ( lookup )

type Hash = Int

data Quad a = Quad !a !a !a !a 
 
data Octo a = Octo {-# UNPACK #-} !(Quad a) {-# UNPACK #-} !(Quad a)

data Bucket k v = Bucket {-# UNPACK #-} !Hash !(FL.FullList k v)

class Store s where
    set :: s a -> Int -> a -> s a
    get :: s a -> Int -> a

instance Store Quad where
    set (Quad a b c d) n x = case n .&. 0x03 of
        0x00 -> Quad x b c d
        0x01 -> Quad a x c d
        0x02 -> Quad a b x d
        _    -> Quad a b c x
    {-# INLINE set #-}
    get (Quad a b c d) n = case n .&. 0x03 of
        0x00 -> a
        0x01 -> b
        0x02 -> c
        _    -> d
    {-# INLINE get #-}

instance Store Octo where
    set (Octo a b) n x = case n .&. 0x04 of
        0x00 -> Octo (set a n x) b
        _    -> Octo a (set b n x)
    {-# INLINE set #-}
    get (Octo a b) n = case n .&. 0x04 of
        0x00 -> get a n
        _    -> get b n
    {-# INLINE get #-}

data Map k v = Empty
             | Tip {-# UNPACK #-} !(Octo (Bucket k v))
             | Bin {-# UNPACK #-} !(Octo (Bucket k v)) !(Map k v) !(Map k v)

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
    go !s Empty = Nothing
    go !s (Tip o) = go' (o `get` s)
    go !s (Bin o l r) = case go' (o `get` s) of
        Nothing -> case s .&. 0x08 of
            0 -> go (s `shiftR` 4) l
            _ -> go (s `shiftR` 4) r  
        just -> just
    go' (Bucket h' fl)
        | h /= h'   = Nothing
        | otherwise = FL.lookup k fl 
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif
