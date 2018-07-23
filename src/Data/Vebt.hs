module Data.Vebt
       ( VEBT (..)
       , empty
       , lookup
       , insert
       ) where

import Prelude hiding (lookup)

import Data.Bits (shiftL, shiftR, (.&.))
import Data.IntMap (IntMap)
import Data.Word (Word64)

import qualified Data.IntMap as IM

-- TODO: better name?
-- TODO: use type-level tricks to make structure safer and store different types.
{- | Implementation of Van Emde Boas Tree
-}
data VEBT v
    -- | Empty tree
    = Empty

    -- | Only minimum in tree with value stored for this @min@.
    | Leaf Word64 !v

    -- | Minimum in whole tree and children. On each depth size of machine word
    -- is divided by 2 but this is not yet expressed in types.
    | Branch Word64 !v (IntMap (VEBT v))
    deriving (Eq, Show)

empty :: VEBT v
empty = Empty

{- | Lookup for element.
-}
lookup :: Word64 -> VEBT v -> Maybe v
lookup = go 64
  where
    -- takes w -- current size of machine word
    go :: Int -> Word64 -> VEBT v -> Maybe v
    go _ _ Empty            = Nothing
    go _ k (Leaf tMin tVal) = if tMin == k then Just tVal else Nothing
    go w k (Branch tMin tVal children) = case k `compare` tMin of
        EQ -> Just tVal
        LT -> Nothing
        GT -> let (hi, lo) = split w k
              in IM.lookup (fromIntegral hi) children >>= go (w `div` 2) lo

{- | Insert for element.
-}
insert :: forall v . Word64 -> v -> VEBT v -> VEBT v
insert = go 64
  where
    -- takes w -- current size of machine word
    go :: Int -> Word64 -> v -> VEBT v -> VEBT v
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

    addNode :: Word64 -> v -> Int -> Maybe (VEBT v) -> VEBT v
    addNode k v _ Nothing  = Leaf k v
    addNode k v w (Just t) = go (w `div` 2) k v t

----------------------------------------------------------------------------
-- utility functions
----------------------------------------------------------------------------

-- | Split number into (hi, lo).
split :: Int -> Word64 -> (Word64, Word64)
split w x = let half = w `shiftR` 1  -- fast division by 2
            in (x `shiftR` half, x .&. ((1 `shiftL` half) - 1))
