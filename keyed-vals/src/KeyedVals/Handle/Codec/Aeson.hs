{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module KeyedVals.Handle.Codec.Aeson (
  -- * newtypes
  ViaAeson (..),
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict',
  encode,
 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import KeyedVals.Handle.Codec (DecodesFrom (..), EncodesAs (..))


{- | A deriving-via helper type for types that implement 'DecodesFrom' and 'EncodesAs'
 using a Aeson's 'FromJSON' and 'ToJSON' type classes.
-}
newtype ViaAeson a = ViaAeson {fromViaAeson :: a}


instance FromJSON a => DecodesFrom (ViaAeson a) where
  decodesFrom = either (Left . Text.pack) (Right . ViaAeson) . eitherDecodeStrict'


instance ToJSON a => EncodesAs (ViaAeson a) where
  encodesAs = LBS.toStrict . encode . fromViaAeson
