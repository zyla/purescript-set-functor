# set-functor

A `Set` that is also a `Functor`. Inspired by <https://hackage.haskell.org/package/set-monad>.

The one from `Data.Set` can't be given a `Functor` instance, because it
requires the `Ord` constraint. This can be fixed by delaying the actual mapping
to the point where the resulting set is demanded. This library does just this.
