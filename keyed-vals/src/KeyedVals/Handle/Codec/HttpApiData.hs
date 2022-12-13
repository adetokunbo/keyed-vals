{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module KeyedVals.Handle.Codec.HttpApiData (
  -- * newtypes
  ViaHttpApiData (..),
) where

import KeyedVals.Handle.Codec (DecodesFrom (..), EncodesAs (..))
import Web.HttpApiData (
  FromHttpApiData (..),
  ToHttpApiData (..),
 )


{- | A deriving-via helper type for types that implement 'DecodesFrom' and 'EncodesAs'
 using a 'FromHttpApiData' and 'ToHttpApiData' type classes.
-}
newtype ViaHttpApiData a = ViaHttpApiData {fromViaHttpApiData :: a}


instance FromHttpApiData a => DecodesFrom (ViaHttpApiData a) where
  decodesFrom = fmap ViaHttpApiData . parseHeader


instance ToHttpApiData a => EncodesAs (ViaHttpApiData a) where
  encodesAs = toHeader . fromViaHttpApiData
