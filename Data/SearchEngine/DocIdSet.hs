{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SearchEngine.DocIdSet
  ( DocId (DocId),
    DocIdSet (..),
    null,
    size,
    empty,
    singleton,
    fromList,
    toList,
    insert,
    delete,
    union,
    unions,
    intersection,
    invariant,
  )
where

import Control.Monad (liftM)
import Control.Monad.ST
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.List (foldl', sortBy)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Sequence ((|>))
import qualified Data.Set as Set
import qualified Data.Vector.Generic as GVec
import qualified Data.Vector.Generic.Mutable as GMVec
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import Data.Word
import Prelude hiding (null)

newtype DocId = DocId {unDocId :: Word32}
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype DocIdSet = DocIdSet (Seq DocId)
  deriving (Eq, Show)

-- represented as a sorted sequence of ids
invariant :: DocIdSet -> Bool
invariant (DocIdSet vec) =
  strictlyAscending (Foldable.toList vec)
  where
    strictlyAscending (a : xs@(b : _)) = a < b && strictlyAscending xs
    strictlyAscending _ = True

size :: DocIdSet -> Int
size (DocIdSet vec) = Seq.length vec

null :: DocIdSet -> Bool
null (DocIdSet vec) = Seq.null vec

empty :: DocIdSet
empty = DocIdSet Seq.empty

singleton :: DocId -> DocIdSet
singleton = DocIdSet . Seq.singleton

fromList :: [DocId] -> DocIdSet
fromList = DocIdSet . Seq.fromList

toList :: DocIdSet -> [DocId]
toList (DocIdSet vec) = Foldable.toList vec

insert :: DocId -> DocIdSet -> DocIdSet
insert x (DocIdSet vec) = DocIdSet $ vec |> x

delete :: DocId -> DocIdSet -> DocIdSet
delete x (DocIdSet vec) =
  case binarySearch vec 0 (Seq.length vec - 1) x of
    (_, False) -> DocIdSet vec
    (i, True) -> case Seq.splitAt i vec of
      (before, after) ->
        DocIdSet (before Seq.>< after)

binarySearch :: Seq DocId -> Int -> Int -> DocId -> (Int, Bool)
binarySearch vec !a !b !key
  | a > b = (a, False)
  | otherwise =
    let mid = (a + b) `div` 2
     in case compare key (Seq.index vec mid) of
          LT -> binarySearch vec a (mid -1) key
          EQ -> (mid, True)
          GT -> binarySearch vec (mid + 1) b key

unions :: [DocIdSet] -> DocIdSet
unions =
  foldl' union empty
    -- a bit more effecient if we merge small ones first
    . sortBy (compare `on` size)

union :: DocIdSet -> DocIdSet -> DocIdSet
union x y
  | null x = y
  | null y = x
union (DocIdSet xs) (DocIdSet ys) = DocIdSet $ xs Seq.>< ys

intersection :: DocIdSet -> DocIdSet -> DocIdSet
intersection x y
  | null x = empty
  | null y = empty
intersection (DocIdSet xs) (DocIdSet ys) = DocIdSet $ xs Seq.>< ys
