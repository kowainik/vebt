{-# LANGUAGE MagicHash #-}

module Data.Vebt
       ( VEBT (..)
       , empty
       , lookup
       , insert
       ) where

import Prelude hiding (lookup)

import Data.Bits (shiftL, shiftR, (.&.))
import Data.IntMap (IntMap)
import GHC.Exts (Int (..), word2Int#)
import GHC.Generics (Generic)
import GHC.Word (Word64 (..))

import qualified Data.IntMap as IM

-- TODO: better name?
-- TODO: use type-level tricks to make structure safer and store different types.
{- | Implementation of Van Emde Boas Tree
-}
data VEBT v
    -- | Empty tree
    = Empty

    -- | Only minimum in tree with value stored for this @min@.
    | Leaf {-# UNPACK #-} !Word64 !v

    -- | Minimum in whole tree and children. On each depth size of machine word
    -- is divided by 2 but this is not yet expressed in types.
    | Branch {-# UNPACK #-} !Word64 !v (IntMap (VEBT v))
    deriving (Eq, Show, Generic)

empty :: VEBT v
empty = Empty

{- | Lookup for element.
-}
lookup :: forall v . Word64 -> VEBT v -> Maybe v
lookup = go W64
  where
    -- takes w -- current size of machine word
    go :: WSize -> Word64 -> VEBT v -> Maybe v
    go _ _ Empty            = Nothing
    go _ k (Leaf tMin tVal) = if tMin == k then Just tVal else Nothing
    go w k (Branch tMin tVal children) = case k `compare` tMin of
        EQ -> Just tVal
        LT -> Nothing
        GT -> let (hi, lo) = split w k
              in imLookup hi >>= go (half w) lo
      where
        imLookup :: Word64 -> Maybe (VEBT v)
        imLookup (W64# i) = IM.lookup (I# (word2Int# i)) children

{- | Insert for element.
-}
insert :: forall v . Word64 -> v -> VEBT v -> VEBT v
insert = go W64
  where
    -- takes w -- current size of machine word
    go :: WSize -> Word64 -> v -> VEBT v -> VEBT v
    go _ k v Empty = Leaf k v
    go w k v (Leaf tMin tVal) = case k `compare` tMin of
        EQ -> Leaf tMin v
        LT -> let (hi, lo) = split w tMin
              in Branch k v $ IM.singleton (fromIntegral hi) $ Leaf lo tVal
        GT -> let (hi, lo) = split w k
              in Branch tMin tVal $ IM.singleton (fromIntegral hi) $ Leaf lo v
    go w k v (Branch tMin tVal children) = case k `compare` tMin of
        EQ -> Branch tMin v children
        LT -> let (hi, lo) = split w tMin
              in Branch k v $ IM.alter (Just . addNode lo tVal w) (fromIntegral hi) children
        GT -> let (hi, lo) = split w k
              in Branch tMin tVal $ IM.alter (Just . addNode lo v w) (fromIntegral hi) children

    addNode :: Word64 -> v -> WSize -> Maybe (VEBT v) -> VEBT v
    addNode k v _ Nothing  = Leaf k v
    addNode k v w (Just t) = go (half w) k v t

----------------------------------------------------------------------------
-- utility functions
----------------------------------------------------------------------------

data WSize = W64 | W32 | W16 | W8 | W4 | W2 | W1

half :: WSize -> WSize
half W64 = W32
half W32 = W16
half W16 = W8
half W8  = W4
half W4  = W2
half W2  = W1
half W1  = W1  -- should be called for W1 in real code
{-# INLINE half #-}

-- Precalculated values
splitVals :: WSize -> (Int, Word64)
splitVals W64 = (32, 4294967295)
splitVals W32 = (16, 65535)
splitVals W16 = (8, 127)
splitVals W8  = (4, 15)
splitVals W4  = (2, 3)
splitVals W2  = (1, 1)
splitVals W1  = (0, 0)
{-# INLINE splitVals #-}

-- | Split number into (hi, lo).
split :: WSize -> Word64 -> (Word64, Word64)
split w x = let (shift, lo) = splitVals w in (x `shiftR` shift, x .&. lo)
{-# INLINE split #-}
