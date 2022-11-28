# keyed-vals

[![GitHub CI](https://github.com/adetokunbo/keyed-vals/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/keyed-vals/actions)
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/keyed-vals/blob/master/LICENSE)

`keyed-vals` aims to provide a narrow client for storing key-value collections
in storage services like [Redis].

Its split into several packages, so the necessary features can be used with
minimal additional dependencies

Packages include:

- an abstract [Handle] that defines the [main interface] along with supporting combinators
- a [redis implementation] of the Handle
- an [in-memory implementation] the Handle


[hackage-deps-badge]:       <https://img.shields.io/hackage-deps/v/keyed-vals.svg>
[hackage-deps]:             <http://packdeps.haskellers.com/feed?needle=keyed-vals>
[hackage-badge]:            <https://img.shields.io/hackage/v/keyed-vals.svg>
[hackage]:                  <https://hackage.haskell.org/package/keyed-vals>
[Handle]:                   <https://jaspervdj.be/posts/2018-03-08-handle-pattern.html>
[Redis]:                    <https://redis.io>
[main interface]:           <https://hackage.haskell.org/package/keyed-vals>
[redis implementation]:     <https://hackage.haskell.org/package/keyed-vals-redis>
[in-memory implementation]: <https://hackage.haskell.org/package/keyed-vals-mem>
