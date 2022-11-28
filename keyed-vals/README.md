# keyed-vals

[![GitHub
CI](https://github.com/adetokunbo/keyed-vals/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/keyed-vals/actions)
[![Stackage
Nightly](http://stackage.org/package/keyed-vals/badge/nightly)](http://stackage.org/nightly/package/keyed-vals)
[![Hackage][hackage-badge]][hackage] [![Hackage
Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/keyed-vals/blob/master/LICENSE)

[keyed-vals](https://hackage.haskell.org/package/keyed-vals) aims to provide a
narrow client for storing key-value collections in storage services like
[Redis].

E.g,

  - [Redis] supports many other features
  - the abstract [Handle] declared in `keyed-vals` just provides combinators that operate on key-value collections stored in some backend
  - so the redis implementation of [Handle] accesses collections in Redis *without* exposing its other features.


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/keyed-vals.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=keyed-vals>
[hackage-badge]:      <https://img.shields.io/hackage/v/keyed-vals.svg>
[hackage]:            <https://hackage.haskell.org/package/keyed-vals>
[Handle]:             <https://jaspervdj.be/posts/2018-03-08-handle-pattern.html>
[Redis]:              <https://redis.io>
