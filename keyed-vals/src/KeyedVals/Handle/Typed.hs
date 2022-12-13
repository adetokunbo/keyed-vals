{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : KeyedVals.Handle.Typed
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides typeclasses, data types and combinators that constrain the @types@ of
keys and values accessed in the key-value store, whilst also linking them to specific
storage paths.
-}
module KeyedVals.Handle.Typed (
  -- * How use this module
  -- $use

  -- * type-and-path-constrained Handle combinators
  TypedKVs,
  countKVs,
  loadFrom,
  loadKVs,
  loadSlice,
  mayLoadFrom,
  modKVs,
  saveTo,
  saveKVs,
  updateKVs,

  -- * link key-value collections to a path
  PathOf (..),
  VaryingPathOf (..),
  rawPath,
  expand,
  prepend,
  append,

  -- * unite @PathOf@/@VaryingPathOf@
  TypedPath (..),
  TypedKey (..),
  pathKey,
  asKey,
  pathOf,

  -- * module re-exports
  module KeyedVals.Handle,
  module KeyedVals.Handle.Codec,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import KeyedVals.Handle (
  Handle,
  HandleErr (..),
  Key,
  Selection (..),
  close,
 )
import qualified KeyedVals.Handle as H
import KeyedVals.Handle.Codec
import Numeric.Natural


{- $use

 This section contains information on how to store data using this library.
 First there is a preamble that shows the directives and imports are used in the
 examples below

 > {\-# LANGUAGE DeriveGeneric #-\}
 > {\-# LANGUAGE DerivingVia #-\}
 > {\-# LANGUAGE OverloadedStrings #-\}
 > {\-# LANGUAGE StandaloneDeriving #-\}
 >
 > import Data.Aeson (FromJSON, ToJSON)
 > import Data.Text (Text)
 > import GHC.Generics (Generic)
 > import KeyedVals.Handle.Codec.Aeson (ViaAeson(..))
 > import KeyedVals.Handle.Codec.HttpApiData (ViaHttpApiData(..))
 > import qualified KeyedVals.Handle.Mem as Mem
 > import KeyedVals.Handle.Typed
 > import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

 Usage is fairly simple: 'PathOf' and possibly a 'VaryingPathOf' instances for
 storable data types are declared. They describe how the data type is encoded
 and decoded and where in the key-value store the data should be saved.

 For example, given this data type:

 > data Person = Person
 >   { name :: Text
 >   , age  :: Int
 >   } deriving (Eq, Show, Generic)

 Suppose each @Person@ is to be stored as JSON, via the @Generic@
 implementation, e.g,

 > instance FromJSON Person
 > instance ToJSON Person

 Also suppose each Person is stored with a Int key. To do that, a @newtype@ of
 @Int@, is defined, e.g,

 > newtype PersonID = newtype PersonID Int
 >   deriving stock (Eq, Show)
 >   deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int

 And then suppose the collection of @Person@s is stored at a specific fixed path
 in the key-value store. E.g, it is to be used as a runtime cache to speed up
 access to person data, so the path @/runtime/cache/persons@ is used.

 To specify all this, we define @DecodesFrom@ and @EncodesAs@ instances for
 @Person@:

 > deriving via (ViaAeson Person) instance DecodesFrom Person
 > deriving via (ViaAeson Person) instance EncodesAs Person

 .. and do the same for @PersonID@:

 > deriving via (ViaHttpApiData Int) instance DecodesFrom PersonID
 > deriving via (ViaHttpApiData Int) instance EncodesAs PersonID

 Then declare a @PathOf@ instance that binds the types together with the path:

 > instance PathOf Person where
 >   type KVPath Person = "/runtime/cache/persons"
 >   type KeyType Person = PersonID
 >   toKey _ = encodesAs

 Note: the @DecodesFrom@ and @EncodesAs@ deriving statements above were
 standalone for illustrative purposes. In most cases, they ought to be part
 of the deriving clause of the data type. E.g,

 > newtype AnotherID :: AnotherID Int
 >   deriving stock (Eq, Show)
 >   deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
 >   deriving (DecodesFrom, EncodesAs) via (ViaHttpApiData Int)

 Now load and fetch @Person@s from a storage backend using the functions in this
 module, e.g:

 > >>> handle <- Mem.new
 > >>> tim = Person { name = "Tim", age = 48 }
 > >>> saveTo handle (Raw 1) tim
 > Right ()
 > >>> loadFrom handle (Raw 1)
 > Right (Person { name = "Tim", age = 48 })
-}


-- | Obtains the actual 'Key' for a given 'TypedKey.Handle.Val' from its 'TypedKey'.
asKey :: forall v. TypedKey v -> Key
asKey (Raw x) = toKey @v Proxy x
asKey (Extended _ x) = toKey @v Proxy x


-- | Obtains the path indicted by a 'TypedPath' as a 'Key'.
pathKey :: forall v. TypedPath v -> Key
pathKey Fixed = rawPath @v Proxy
pathKey (Variable part) = modifyPath @v Proxy part $ rawPath @v Proxy


-- | Derives the 'TypedPath' corresponding to a 'TypedKey'.
pathOf :: forall v. TypedKey v -> TypedPath v
pathOf (Raw _) = Fixed
pathOf (Extended part _) = Variable part


{- | Represents a related group of @values@ each stored using a key of type
 @'KeyType' <value type>@
-}
type TypedKVs value = Map (KeyType value) value


{- | Links the storage path of a group of key-values to the types of the key and
   value.
-}
class KnownSymbol (KVPath value) => PathOf value where
  -- * the storage path where the key-values are saved
  type KVPath value :: Symbol


  -- * the type of keys in this group of key-values
  type KeyType value


  -- * specifies how to convert an 'KeyType' to a 'Key'
  toKey :: Proxy value -> KeyType value -> Key


{- | Allow the storage path specifed by @'PathOf'@ to vary so that related
  groups of key-values may be stored in similar, related paths.
-}
class PathOf value => VaryingPathOf value where
  -- * @PathVar@ is specified to modify the path
  type PathVar value


  -- * Combines the raw 'KVPath' and @PathVar to obtain a new path.
  modifyPath :: Proxy value -> PathVar value -> Key -> Key


-- | Supports implementation of 'modifyPath' via substitution of @{}@ within the 'KVPath'.
expand :: EncodesAs a => a -> Key -> Key
expand x template =
  let (prefix, afterPre) = B.breakSubstring braces template
      suffix = B.drop (B.length braces) afterPre
      result = prefix <> encodesAs x <> suffix
   in if B.isPrefixOf braces afterPre then result else template


{- | Supports implementation of 'modifyPath'

Intended for used within the 'KVPath' of instances of 'VaryingPathOf', indicates where
 a variable should be substituted
-}
braces :: B.ByteString
braces = "{}"


-- | Supports implementaton of 'modifyPath'.
append :: EncodesAs a => Key -> a -> Key -> Key
append sep x template = template <> sep <> encodesAs x


-- | Supports implementaton of 'modifyPath'
prepend :: EncodesAs a => Key -> a -> Key -> Key
prepend sep x template = encodesAs x <> sep <> template


{- | A phantom type indicating either an instance of @'PathOf'@ or of
   @'VaryingPathOf'@.

 Allows combinators with similar behaviour for either to be defined just once,
 rather than separately for each typeclass.
-}
data TypedPath v where
  Fixed :: (PathOf v) => TypedPath v
  Variable :: (VaryingPathOf v) => PathVar v -> TypedPath v


-- | Similar to 'TypedPath', but includes an actual key along with the phantom type.
data TypedKey v where
  Raw :: (PathOf v) => KeyType v -> TypedKey v
  Extended :: (VaryingPathOf v) => PathVar v -> KeyType v -> TypedKey v


-- | Obtain the raw path to key-values that implement 'PathOf'.
rawPath :: forall value. PathOf value => Proxy value -> Key
rawPath _ = C8.pack $ symbolVal @(KVPath value) Proxy


-- | Like 'mayLoadFrom', but fails with 'Gone' if the value is missing.
loadFrom ::
  forall a m.
  (Monad m, DecodesFrom a) =>
  Handle m ->
  TypedKey a ->
  m (Either HandleErr a)
loadFrom h aKey =
  let outer = pathKey $ pathOf aKey
      inner = asKey aKey
      full = outer <> "//" <> inner
   in H.loadFrom h outer inner <&> decodeOrGone' full


{- | Like @'KeyedValues.Handle.loadVal'@ with the key, path and value
 constrained by @'TypedKey'@
-}
mayLoadFrom ::
  forall a m.
  (Monad m, DecodesFrom a, PathOf a) =>
  Handle m ->
  TypedKey a ->
  m (Either HandleErr (Maybe a))
mayLoadFrom h aKey =
  let outer = pathKey $ pathOf aKey
      inner = asKey aKey
   in H.loadFrom h outer inner <&> decodeOr'


{- | Like @'KeyedValues.Handle.saveTo'@ with the key, path and value constrained
 by @'TypedKey'@
-}
saveTo ::
  (Monad m, EncodesAs a, PathOf a) =>
  Handle m ->
  TypedKey a ->
  a ->
  m (Either HandleErr ())
saveTo h aKey someKVs =
  let outer = pathKey $ pathOf aKey
      inner = asKey aKey
   in H.saveTo h outer inner $ encodesAs someKVs


{- | Like @'KeyedValues.Handle.loadKVs'@ with the path and key values constrained
 by @'TypedPath'@
-}
loadKVs ::
  ( Monad m
  , DecodesFrom a
  , DecodesFrom (KeyType a)
  , Ord (KeyType a)
  ) =>
  Handle m ->
  TypedPath a ->
  m (Either HandleErr (TypedKVs a))
loadKVs h k = H.loadKVs h (pathKey k) >>= pure . orDecodeKVs


{- | Like @'KeyedValues.Handle.updateKVs'@ with the path and key-values
 constrained by @'TypedPath'@
-}
updateKVs ::
  (Monad m, EncodesAs a, EncodesAs (KeyType a), Ord (KeyType a)) =>
  Handle m ->
  TypedPath a ->
  TypedKVs a ->
  m (Either HandleErr ())
updateKVs h aKey = updateEncodedKVs h $ pathKey aKey


{- | Like @'KeyedValues.Handle.savedKVs'@ with the path and key-values
 constrained by @'TypedPath'@
-}
saveKVs ::
  (Monad m, EncodesAs a, EncodesAs (KeyType a), Ord (KeyType a)) =>
  Handle m ->
  TypedPath a ->
  TypedKVs a ->
  m (Either HandleErr ())
saveKVs h k = saveEncodedKVs h $ pathKey k


-- | Combines 'saveKVs' and 'loadKVs'
modKVs ::
  ( Monad m
  , EncodesAs a
  , EncodesAs (KeyType a)
  , DecodesFrom a
  , DecodesFrom (KeyType a)
  , Ord (KeyType a)
  ) =>
  (TypedKVs a -> TypedKVs a) ->
  Handle m ->
  TypedPath a ->
  m (Either HandleErr ())
modKVs modDict h aKey = do
  let key = pathKey aKey
  H.loadKVs h key >>= (pure . orDecodeKVs) >>= \case
    Left err -> pure $ Left err
    Right d -> saveKVs h aKey $ modDict d


{- | Like @'KeyedValues.Handle.loadSlice'@ with the path and key-values
 constrained by @'TypedPath'@
-}
loadSlice ::
  forall m a.
  ( Monad m
  , DecodesFrom a
  , PathOf a
  , DecodesFrom (KeyType a)
  , Ord (KeyType a)
  ) =>
  Handle m ->
  TypedPath a ->
  NonEmpty (KeyType a) ->
  m (Either HandleErr (TypedKVs a))
loadSlice h aKey keys = do
  let key = pathKey aKey
      selection = AllOf $ fmap (toKey @a Proxy) keys
  H.loadSlice h key selection >>= pure . orDecodeKVs


orDecodeKVs ::
  (Ord a, DecodesFrom a, DecodesFrom b) =>
  Either HandleErr H.ValsByKey ->
  Either HandleErr (Map a b)
orDecodeKVs = either Left decodeKVs


{- | Like @'KeyedValues.Handle.countKVs'@ with the path and key-values
 constrained by @'TypedPath'@
-}
countKVs ::
  forall a m.
  ( Monad m
  , Ord (KeyType a)
  ) =>
  Handle m ->
  TypedPath a ->
  m (Either HandleErr Natural)
countKVs h k = H.countKVs h $ pathKey k
