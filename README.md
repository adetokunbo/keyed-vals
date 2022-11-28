# keyed-vals

[![GitHub CI](https://github.com/adetokunbo/keyed-vals/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/keyed-vals/actions)
[![Stackage Nightly](http://stackage.org/package/keyed-vals/badge/nightly)](http://stackage.org/nightly/package/keyed-vals)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/keyed-vals/blob/master/LICENSE)

[keyed-vals](https://hackage.haskell.org/package/keyed-vals) aims
to provide a 'narrow' client of [Redis](https://redis.io).

I.e, __Redis__ supports many features; the abstract __Handle__ declared in
`keyed-vals` just supports operations that access collections of values stored
by keys (aka: 'dict', 'map', hash', 'object' and even 'context')


## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

```

[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/keyed-vals.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=keyed-vals>
[hackage-badge]:      <https://img.shields.io/hackage/v/keyed-vals.svg>
[hackage]:            <https://hackage.haskell.org/package/keyed-vals>
[KEYS]:               <https://redis.io/commands/keys>
