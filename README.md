# set-functor

A `Set` that is also a `Functor`.

The one from `Data.Set` can't be given a `Functor` instance, because it
requires the `Ord` constraint.
