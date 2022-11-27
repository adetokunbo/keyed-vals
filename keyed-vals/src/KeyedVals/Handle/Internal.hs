{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : KeyedVals.Handle.Internal
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Declares the abstract @'Handle'@
-}
module KeyedVals.Handle.Internal (
  -- * types used in the @Handle@ functions
  HandleErr (..),
  Glob,
  mkGlob,
  Selection (..),

  -- * the abstract @Handle@
  Handle (..),

  -- * aliases used in the 'Handle' functions
  Key,
  Val,
  ValsByKey,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Redis.Glob (validate)


-- | A handle for accessing the 'ValsByKey' store.
data Handle m = Handle
  { hLoadVal :: !(Key -> m (Either HandleErr (Maybe Val)))
  , hSaveVal :: !(Key -> Val -> m (Either HandleErr ()))
  , hCountKVs :: !(Key -> m (Either HandleErr Natural))
  , hLoadKVs :: !(Key -> m (Either HandleErr ValsByKey))
  , hSaveKVs :: !(Key -> ValsByKey -> m (Either HandleErr ()))
  , hUpdateKVs :: !(Key -> ValsByKey -> m (Either HandleErr ()))
  , hLoadFrom :: !(Key -> Key -> m (Either HandleErr (Maybe Val)))
  , hSaveTo :: !(Key -> Key -> Val -> m (Either HandleErr ()))
  , hLoadSlice :: !(Key -> Selection -> m (Either HandleErr ValsByKey))
  , hDeleteSelected :: !(Selection -> m (Either HandleErr ()))
  , hDeleteSelectedKVs :: !(Key -> Selection -> m (Either HandleErr ()))
  , hClose :: !(m ())
  }


-- | Represents the errors that might arise in 'Handle' functions
data HandleErr
  = ConnectionClosed
  | Unanticipated !Text
  | NotDecoded !Text
  | BadKey
  | Gone !Key
  deriving (Eq, Show)


-- | Represents ways of restricting the keys used in a @'ValsByKey'@
data Selection
  = -- | any keys that match the glob pattern
    Match !Glob
  | -- | any of the specified keys
    AllOf !(NonEmpty Key)
  deriving (Eq, Show)


-- | Represents a redis glob use to select keys
newtype Glob = Glob ByteString
  deriving (Eq, Show)


{- | constructor for a 'Glob'

returns 'Nothing' when the pattern is invalid
-}
mkGlob :: ByteString -> Maybe Glob
mkGlob = fmap (Glob . LB.toStrict) . validate . LB.fromStrict


instance Exception HandleErr


-- | Represents a key used to store a 'Value'.
type Key = ByteString


-- | Represents a value stored in the service.
type Val = ByteString


-- | Represents a related group of @'Value'@s stored by @'Key'@.
type ValsByKey = Map Key Val
