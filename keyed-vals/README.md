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
  - so the [redis implementation] of [Handle] accesses collections in Redis *without* exposing its other features.

## Example

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import KeyedVals.Handle.Codec.Aeson (ViaAeson(..))
import KeyedVals.Handle.Codec.HttpApiData (ViaHttpApiData(..))
import qualified KeyedVals.Handle.Mem as Mem
import KeyedVals.Handle.Typed
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

{- Usage is fairly simple:

- Declare 'PathOf' and possibly a 'VaryingPathOf' instance for
  storable data types.

They describe how the data type is encoded and decoded and where in the
key-value store the data should be saved.

For example, given the following data type:
-}
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Eq, Show, Generic)

{- Suppose each @Person@ is to be stored as JSON, via the @Generic@
implementation, e.g,
-}
instance FromJSON Person
instance ToJSON Person

{- Also suppose each Person is stored with an @Int@ key. To enable that,
define a @newtype@ of @Int@, e.g,
-}
newtype PersonID = newtype PersonID Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int

{- And then suppose the collection of @Person@s is stored at a specific fixed path
in the key-value store. E.g, it is to be used as a runtime cache to speed up
access to person data, so the path @/runtime/cache/persons@ is used.

To specify all of this, first define @DecodesFrom@ and @EncodesAs@ instances for
@Person@:
-}
deriving via (ViaAeson Person) instance DecodesFrom Person
deriving via (ViaAeson Person) instance EncodesAs Person

{- .. and do the same for @PersonID@: -}
deriving via (ViaHttpApiData Int) instance DecodesFrom PersonID
deriving via (ViaHttpApiData Int) instance EncodesAs PersonID


{- Then declare a @PathOf@ instance that binds the types together with the path: -}
instance PathOf Person where
  type KVPath Person = "/runtime/cache/persons"
  type KeyType Person = PersonID
  toKey _ = encodesAs

{- Note: the @DecodesFrom@ and @EncodesAs@ deriving statements above were
standalone for illustrative purposes. In most cases, they ought to be part
of the deriving clause of the data type. E.g,
-}
newtype AnotherID = AnotherID Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
  deriving (DecodesFrom, EncodesAs) via (ViaHttpApiData Int)

{- Now load and fetch @Person@s from a storage backend using the functions in this
module, e.g:

>>> handle <- Mem.new
>>> tim = Person { name = "Tim", age = 48 }
>>> saveTo handle (Raw 1) tim
Right ()
>>> loadFrom handle (Raw 1)
Right (Person { name = "Tim", age = 48 })

-}
```

[hackage-deps-badge]:   <https://img.shields.io/hackage-deps/v/keyed-vals.svg>
[hackage-deps]:         <http://packdeps.haskellers.com/feed?needle=keyed-vals>
[hackage-badge]:        <https://img.shields.io/hackage/v/keyed-vals.svg>
[hackage]:              <https://hackage.haskell.org/package/keyed-vals>
[Handle]:               <https://hackage.haskell.org/package/keyed-vals-0.1.0.0/docs/KeyedVals-Handle.html>
[Redis]:                <https://redis.io>

[redis implementation]: <https://hackage.haskell.org/package/keyed-vals-redis>
