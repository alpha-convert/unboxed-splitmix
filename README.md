# Splittable Random Number Generation with Unboxed Types

This is verbatim reimplementation of Jane Street's [Splittable_random](https://github.com/janestreet/splittable_random/) RNG library, which is in turn an implementation of the [splitmix paper](https://gee.cs.oswego.edu/dl/papers/oopsla14.pdf).

The *only* difference between this library and Splittable_random is that this one uses unboxed types and locals from the Jane Street bleeding-edge compiler branch. The implementation of the splitmix algorithm in Splittable_random uses the following type to encode the RNG state:
```ocaml
type t = { mutable seed : int64; odd_gamma : int64 }
```
Because `int64` has kind `value`, this data structure includes two un-needed boxes for the two fields, and *every* arithmetic or logical operation the library does to compute on `int64`s must unbox and re-allocate these values.

This library instead uses the following type, which lets us implement all of the bit arithmetic with machine instructions and no allocation.
```ocaml
type t = { mutable seed : int64#; odd_gamma : int64# }
```

## Dependencies

Builds on [OCaml with Jane Street extensions](https://github.com/janestreet/opam-repository/tree/with-extensions)