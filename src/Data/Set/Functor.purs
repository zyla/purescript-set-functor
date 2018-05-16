module Data.Set.Functor
  ( Set
  , fromFoldable
  , toUnfoldable
  , empty
  , isEmpty
  , singleton
  , insert
  , member
  , delete
  , size
  , findMin
  , findMax
  , union
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Unfoldable (class Unfoldable)

data Set a =
  Plain (Set.Set a) | Mapped (Exists (MappedF a))

data MappedF a b = MappedF (Set.Set b) (b -> a)

runSet :: forall a r. (Set.Set a -> r) -> (forall b. Set.Set b -> (b -> a) -> r) -> Set a -> r
runSet k1 _  (Plain set) = k1 set
runSet _  k2 (Mapped ex) = runExists (\(MappedF set fn) -> k2 set fn) ex

mkMapped :: forall a b. Set.Set b -> (b -> a) -> Set a
mkMapped set fn = Mapped (mkExists (MappedF set fn))

mapSet :: forall a b. (a -> b) -> Set a -> Set b
mapSet f = runSet (\set -> mkMapped set f) (\set g -> mkMapped set (f <<< g))

fromSet :: forall a. Set.Set a -> Set a
fromSet = Plain

toSet :: forall a. Ord a => Set a -> Set.Set a
toSet = runSet id (\set fn -> Set.map fn set)

instance functorSet :: Functor Set where
  map = mapSet

-------------------------------------------------------------------

fromFoldable :: forall f a. Foldable f => Ord a => f a -> Set a
fromFoldable = fromSet <<< Set.fromFoldable

toUnfoldable :: forall f a. Unfoldable f => Ord a => Set a -> f a
toUnfoldable = Set.toUnfoldable <<< toSet

empty :: forall a. Set a
empty = fromSet Set.empty

isEmpty :: forall a. Set a -> Boolean
isEmpty = runSet Set.isEmpty (\set _ -> Set.isEmpty set)

singleton :: forall a. a -> Set a
singleton = fromSet <<< Set.singleton

insert :: forall a. Ord a => a -> Set a -> Set a
insert x = fromSet <<< Set.insert x <<< toSet

member :: forall a. Ord a => a -> Set a -> Boolean
member x = Set.member x <<< toSet

delete :: forall a. Ord a => a -> Set a -> Set a
delete x = fromSet <<< Set.delete x <<< toSet

size :: forall a. Set a -> Int
size = runSet Set.size (\set _ -> Set.size set)

findMin :: forall a. Ord a => Set a -> Maybe a
findMin = Set.findMin <<< toSet

findMax :: forall a. Ord a => Set a -> Maybe a
findMax = Set.findMax <<< toSet

union :: forall a. Ord a => Set a -> Set a -> Set a
union s1 s2 = fromSet (Set.union (toSet s1) (toSet s2))

unions :: forall f a. Functor f => Foldable f => Ord a => f (Set a) -> Set a
unions = fromSet <<< Set.unions <<< map toSet

difference :: forall a. Ord a => Set a -> Set a -> Set a
difference s1 s2 = fromSet (Set.difference (toSet s1) (toSet s2))

subset :: forall a. Ord a => Set a -> Set a -> Boolean
subset s1 s2 = Set.subset (toSet s1) (toSet s2)

properSubset :: forall a. Ord a => Set a -> Set a -> Boolean
properSubset s1 s2 = Set.properSubset (toSet s1) (toSet s2)

intersection :: forall a. Ord a => Set a -> Set a -> Set a
intersection s1 s2 = fromSet (Set.intersection (toSet s1) (toSet s2))
