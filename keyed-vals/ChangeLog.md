# Revision history for keyed-vals

`keyed-vals` uses [PVP Versioning][1].

## 0.2.0.0 -- 2022-12-15

Changed

* generalized encoding and decoding using [KeyedVals.Handle.Codec][]; this
  replaces KeyedVals.Handle.Aeson


Added

* support for encoding and decoding typed keys and values constrained to
  specific paths in the key-value store in [KeyedVals.Handle.Typed][]

## 0.1.0.0 -- 2022-11-28

* Initial version.

[1]: https://pvp.haskell.org
[KeyedVals.Handle.Typed]: https://hackage.haskell.org/package/keyed-vals/docs/KeyedVals-Handle.Typed.html
[KeyedVals.Handle.Codec]: https://hackage.haskell.org/package/keyed-vals/docs/KeyedVals-Handle.Codec.html
