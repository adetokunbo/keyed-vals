{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Functions that help use Aeson to load and save JSON data using the
dictionary service
-}
module KeyedVals.Handle.Aeson (
  -- * decode/encode support
  decodeOr,
  decodeOr',
  decodeOrGone,
  decodeOrGone',
  jsonVal,
  jsonKey,
  webKey,
  appendWebKey,
  substWebKey,
  prependWebKey,

  -- * decode @ValsByKey@
  decodeJsonKeyKVs,
  decodeWebKeyKVs,

  -- * save @ValsByKey@ using a @Handle@
  saveKVs,
  saveKVs',
  saveJsonKeyKVs,
  saveHttpApiKVs,
  updateHttpApiKVs,
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict',
  encode,
 )
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import KeyedVals.Handle.Internal
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


-- | Encode JSON as a remote value.
jsonVal :: ToJSON a => a -> Val
jsonVal = encodeJSON


webKey :: ToHttpApiData a => a -> Key
webKey = toHeader


substWebKey :: ToHttpApiData a => a -> Key -> Key
substWebKey x template =
  let (prefix, afterPre) = B.breakSubstring mustache template
      suffix = B.drop (B.length mustache) afterPre
      result = prefix <> webKey x <> suffix
   in if B.isPrefixOf mustache afterPre then result else template


appendWebKey :: ToHttpApiData a => Key -> a -> Key -> Key
appendWebKey sep x template = template <> sep <> webKey x


prependWebKey :: ToHttpApiData a => Key -> a -> Key -> Key
prependWebKey sep x template = webKey x <> sep <> template


mustache :: ByteString
mustache = "{}"


jsonKey :: ToJSON a => a -> Key
jsonKey = encodeJSON


decodeOrGone ::
  FromJSON b =>
  Key ->
  Maybe Val ->
  Either HandleErr b
decodeOrGone key x =
  case decodeOr NotDecoded x of
    Left err -> Left err
    Right mb -> maybe (Left $ Gone key) Right mb


decodeOrGone' ::
  FromJSON b =>
  Key ->
  Either HandleErr (Maybe Val) ->
  Either HandleErr b
decodeOrGone' key = either Left $ decodeOrGone key


decodeOr' ::
  FromJSON b =>
  Either HandleErr (Maybe Val) ->
  Either HandleErr (Maybe b)
decodeOr' = either Left (decodeOr NotDecoded)


-- | Decode a JSON value, transforming decode errors to type @err@ if they occur.
decodeOr ::
  (FromJSON a) =>
  (Text -> err) ->
  Maybe Val ->
  Either err (Maybe a)
decodeOr f = maybe (pure Nothing) (firstEither (f . Text.pack) . eitherDecodeStrict')


{- | Decode a @ValsByKey@ serialized as JSON.

Both the key and value types are valid to deserialize as JSON.
-}
decodeJsonKeyKVs ::
  (Ord a, FromJSON a, FromJSON b) =>
  (Text -> c) ->
  ValsByKey ->
  Either c (Map a b)
decodeJsonKeyKVs f = firstEither f . decodeKVs' decoder
  where
    decoder = firstEither Text.pack . eitherDecodeStrict'


{- | Decode a @ValsByKey@ serialized as JSON.

- The key type is deserialized as HttpApiData.
- The value type is valid to deserialize as JSON.
-}
decodeWebKeyKVs ::
  (Ord a, FromHttpApiData a, FromJSON b) =>
  (Text -> c) ->
  ValsByKey ->
  Either c (Map a b)
decodeWebKeyKVs f = firstEither f . decodeKVs' parseHeader


{- | Decode a @ValsByKey@ with values serialized as JSON.

The value type is deserialized as JSON
-}
decodeKVs' ::
  (Ord a, FromJSON b) =>
  (Val -> Either Text a) ->
  ValsByKey ->
  Either Text (Map a b)
decodeKVs' decoder =
  let step _ _ (Left x) = Left x
      step k v (Right m) = case (decoder k, eitherDecodeStrict' v) of
        (Left x, _) -> Left x
        (_, Left y) -> Left $ Text.pack y
        (Right k', Right v') -> Right $ Map.insert k' v' m
   in Map.foldrWithKey step (Right Map.empty)


{- | Encode a @ValsByKey@ serialized as JSON.

- Both @'Key's@ and @'Val's@ are encoded using 'ToJSON'
-}
saveJsonKeyKVs ::
  (Ord a, ToJSON a, ToJSON b, Monad m) =>
  (HandleErr -> err) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveJsonKeyKVs f = saveKVs f encodeJSON


{- | Encode a @ValsByKey@ serialized as JSON, completely replacing the current value if present.

- @'Key's@ encode using 'HttpApiData'
- @'Val's@ encode using 'ToJSON'
-}
saveHttpApiKVs ::
  (Ord a, ToHttpApiData a, ToJSON b, Monad m) =>
  (HandleErr -> err) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveHttpApiKVs fromHandleErr = saveKVs fromHandleErr toHeader


-- | Like 'saveHttpApiKVs', but updates the keys rather than completely replacing it.
updateHttpApiKVs ::
  (Ord a, ToHttpApiData a, ToJSON b, Monad m) =>
  (HandleErr -> err) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
updateHttpApiKVs fromHandleErr = saveOrUpdateKVs True fromHandleErr toHeader


-- | Like 'saveKVs', with 'HandleErr' as the error type.
saveKVs' ::
  (Ord a, ToJSON b, Monad m) =>
  (a -> Val) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either HandleErr ())
saveKVs' = saveKVs id


{- |  Encode a 'Map' as a 'ValsByKey' with the @'Val's@ encoded as JSON.

- The @Map@ keys is encoded as @'Key's@ using the provided function,
- The @Map@ values are encoded as @'Val's@ by conversion to JSON.
- 'HandleErr' may be transformed to different error type
-}
saveKVs ::
  (Ord a, ToJSON b, Monad m) =>
  (HandleErr -> err) ->
  (a -> Val) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveKVs = saveOrUpdateKVs False


{- | Encode a 'Map' as a 'ValsByKey' with the @'Val's@ encoded as JSON.

- The @Map@ keys is encoded as @'Key's@ using the provided function,
- The @Map@ values are encoded as @'Val's@ by conversion to JSON.
- Allows 'HandleErr' to be converted to a different error type.
-}
saveOrUpdateKVs ::
  (Ord a, ToJSON b, Monad m) =>
  -- | when @True@, the dict is updated
  Bool ->
  (HandleErr -> err) ->
  (a -> Val) ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveOrUpdateKVs _ _ _ _ _ dict | Map.size dict == 0 = pure $ Right ()
saveOrUpdateKVs update toErr fromKey h key dict =
  let asRemote =
        Map.fromList
          . fmap (bimap fromKey encodeJSON)
          . Map.toList
      saver = if update then hUpdateKVs else hSaveKVs
   in fmap (firstEither toErr) $ saver h key $ asRemote dict


firstEither :: (err1 -> err2) -> Either err1 b -> Either err2 b
firstEither f = either (Left . f) Right


-- | Encode JSON as a remote value.
encodeJSON :: ToJSON a => a -> Val
encodeJSON = LBS.toStrict . encode
