module Data.Vebt
       ( VEBT (..)
       , empty
       , lookup
       , insert
       ) where

import Prelude hiding (lookup)

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
    -- is diveded by 2 but this is not yet expressed in types.
    | Branch Word64 (IntMap (VEBT v))
    deriving (Eq, Show)

empty :: VEBT v
empty = Empty

{- | Lookup for element.
-}
lookup :: Word64 -> VEBT v -> Maybe v
lookup k = go 6  -- 6 as in 2^6 = 64
  where
    -- takes k -- current size of machine word
    go :: Int -> VEBT v -> Maybe v
    go = error "Not implemented!"

{- | Lookup for element.
-}
insert :: Word64 -> v -> VEBT v -> VEBT v
insert k v = go 6  -- 6 as in 2^6 = 64
  where
    -- takes k -- current size of machine word
    go :: Int -> VEBT v -> VEBT v
    go = error "Not implemented!"
