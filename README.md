# set-functor

A `Set` that is also a `Functor`. Inspired by <https://hackage.haskell.org/package/set-monad>.

The `Set` data type from `Data.Set` can't be given a `Functor` instance, because
its `map` function has the following type signature:


```purescript
map :: forall a b. Ord b => (a -> b) -> Set a -> Set b
```

The `Ord` constraint is required to construct the resulting set.

The `Set` from this library has a `Functor` instance. The `map` function just
stores the function in a data structure. The actual mapping is done once the
set is demanded. The result is stored in the original structure, so it's shared
between subsequent queries of the same set. For example, this code:

```purescript
import Data.Set.Functor as SetF

let set = map (plus 1) (SetF.fromFoldable [1,2,3,4])
in Tuple (SetF.member 1 set) (SetF.member 2 set)
```

will map over the set only once, in the first `member` call evaluated.

As an interesting side effect of the implementation, `map` calls are fused if
the set is not demanded in the meantime. For example, given this set:

```purescript
let set = map (plus 1) $ map (plus 2) $ SetF.fromFoldable [1,2,3,4]
```

The first query will map over the original set only once, not twice.
