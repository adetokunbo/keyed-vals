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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : KeyedVals.Handle.Catalog
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module KeyedVals.Handle.Catalog (
  -- * @ToKey@, @ModKVPath@ and related combinators
  ToKey (..),
  ModKVPath (..),
  loadFrom,
  mayLoadFrom,
  loadKVs,
  loadSlice,
  modKVs,
  saveTo,
  saveKVs,
  updateKVs,
  countKVs,
  TypedPath (..),
  TypedKey (..),

  -- * aliases
  TypedKVs,
) where

import Data.Aeson (FromJSON, ToJSON)
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
 )
import qualified KeyedVals.Handle as H
import KeyedVals.Handle.Aeson (
  decodeOr',
  decodeOrGone',
  decodeWebKeyKVs,
  jsonVal,
  saveHttpApiKVs,
  updateHttpApiKVs,
 )
import Numeric.Natural
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


kvPathOf :: forall a. TypedPath a -> Key
kvPathOf (Raw proxy) = rawPath proxy
kvPathOf (Mod proxy part) = modKVPath proxy part $ rawPath proxy


fullPathOf :: forall a. TypedKey a -> TypedPath a
fullPathOf (Basic _) = Raw @a Proxy
fullPathOf (Extended part _) = Mod @a Proxy part


fromTyped :: forall a. TypedKey a -> Key
fromTyped (Basic x) = toKey @a Proxy x
fromTyped (Extended _ x) = toKey @a Proxy x


class KnownSymbol (KVPath v) => ToKey v where
  type KVPath v :: Symbol
  type Inner v
  toKey :: Proxy v -> Inner v -> Key


class ToKey v => ModKVPath v where
  type PathVar v
  modKVPath :: Proxy v -> PathVar v -> Key -> Key


data TypedKey a where
  Basic :: (ToKey a) => Inner a -> TypedKey a
  Extended :: (ModKVPath a) => PathVar a -> Inner a -> TypedKey a


data TypedPath a where
  Raw :: (ToKey a) => Proxy a -> TypedPath a
  Mod :: (ModKVPath a) => Proxy a -> PathVar a -> TypedPath a


rawPath :: forall v. ToKey v => Proxy v -> Key
rawPath _ = C8.pack $ symbolVal @(KVPath v) Proxy


loadFrom ::
  forall a m.
  (Monad m, FromJSON a) =>
  Handle m ->
  TypedKey a ->
  m (Either HandleErr a)
loadFrom h aKey =
  let outer = kvPathOf $ fullPathOf aKey
      inner = fromTyped aKey
      full = outer <> "//" <> inner
   in H.loadFrom h outer inner <&> decodeOrGone' full


mayLoadFrom ::
  forall a m.
  (Monad m, FromJSON a, ToKey a) =>
  Handle m ->
  TypedKey a ->
  m (Either HandleErr (Maybe a))
mayLoadFrom h aKey =
  let outer = kvPathOf $ fullPathOf aKey
      inner = fromTyped aKey
   in H.loadFrom h outer inner <&> decodeOr'


saveTo ::
  (Monad m, ToJSON a, ToKey a) =>
  Handle m ->
  TypedKey a ->
  a ->
  m (Either HandleErr ())
saveTo h aKey aDict =
  let outer = kvPathOf $ fullPathOf aKey
      inner = fromTyped aKey
   in H.saveTo h outer inner $ jsonVal aDict


type TypedKVs a = Map (Inner a) a


loadKVs ::
  ( Monad m
  , FromJSON a
  , FromHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  Handle m ->
  TypedPath a ->
  m (Either HandleErr (TypedKVs a))
loadKVs h k = do
  H.loadKVs h (kvPathOf k) >>= \case
    Left err -> pure $ Left err
    Right d -> pure $ decodeWebKeyKVs NotDecoded d


updateKVs ::
  (Monad m, ToJSON a, ToHttpApiData (Inner a), Ord (Inner a)) =>
  Handle m ->
  TypedPath a ->
  TypedKVs a ->
  m (Either HandleErr ())
updateKVs h aKey = updateHttpApiKVs id h $ kvPathOf aKey


saveKVs ::
  (Monad m, ToJSON a, ToHttpApiData (Inner a), Ord (Inner a)) =>
  Handle m ->
  TypedPath a ->
  TypedKVs a ->
  m (Either HandleErr ())
saveKVs h k = saveHttpApiKVs id h $ kvPathOf k


modKVs ::
  ( Monad m
  , ToJSON a
  , FromJSON a
  , FromHttpApiData (Inner a)
  , ToHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  (TypedKVs a -> TypedKVs a) ->
  Handle m ->
  TypedPath a ->
  m (Either HandleErr ())
modKVs modDict h aKey = do
  let key = kvPathOf aKey
  H.loadKVs h key >>= \case
    Left err -> pure $ Left err
    Right loaded -> case decodeWebKeyKVs NotDecoded loaded of
      Left err -> pure $ Left err
      Right d -> saveHttpApiKVs id h key $ modDict d


loadSlice ::
  forall m a.
  ( Monad m
  , FromJSON a
  , ToKey a
  , FromHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  Handle m ->
  TypedPath a ->
  NonEmpty (Inner a) ->
  m (Either HandleErr (TypedKVs a))
loadSlice h aKey keys = do
  let key = kvPathOf aKey
      selection = AllOf $ fmap (toKey @a Proxy) keys
  H.loadSlice h key selection >>= \case
    Left err -> pure $ Left err
    Right d -> pure $ decodeWebKeyKVs NotDecoded d


countKVs ::
  forall a m.
  ( Monad m
  , ModKVPath a
  , Ord (Inner a)
  ) =>
  Handle m ->
  TypedPath a ->
  m (Either HandleErr Natural)
countKVs h k = H.countKVs h $ kvPathOf k
