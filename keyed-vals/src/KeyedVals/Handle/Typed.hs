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
keys and values accessed in the key-value store, while linking them to specific
storage paths.
-}
module KeyedVals.Handle.Typed (
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
  subst,
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


-- | Suppports implementation of 'toKey' via substitution of 'braces'
subst :: EncodesAs a => a -> Key -> Key
subst x template =
  let (prefix, afterPre) = B.breakSubstring braces template
      suffix = B.drop (B.length braces) afterPre
      result = prefix <> encodesAs x <> suffix
   in if B.isPrefixOf braces afterPre then result else template


-- | Used to indicates where a variable should be substitued by 'subst'
braces :: B.ByteString
braces = "{}"


-- | Supports implementaton of 'toKey'
append :: EncodesAs a => Key -> a -> Key -> Key
append sep x template = template <> sep <> encodesAs x


-- | Supports implementaton of 'toKey'
prepend :: EncodesAs a => Key -> a -> Key -> Key
prepend sep x template = encodesAs x <> sep <> template


{- | Allow the storage path specifed by @'PathOf'@ to vary so that related
  groups of key-values may be stored in similar, related paths.
-}
class PathOf value => VaryingPathOf value where
  -- * @PathVar@ is specified to modify the path
  type PathVar value


  -- * Combines the raw 'KVPath' and @PathVar to obtain a new path.
  modifyPath :: Proxy value -> PathVar value -> Key -> Key


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
