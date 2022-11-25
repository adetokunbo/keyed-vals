{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Dict.Handle.Internal
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Declares the abstract @'Handle'@
-}
module Dict.Handle.Internal (
  -- * error type for @Handle@ functions
  DictErr (..),

  -- * the abstract @Handle@
  Handle (..),

  -- * aliases used in the 'Handle' functions
  Key,
  Value,
  Dict,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Numeric.Natural (Natural)


-- | A handle for accessing the 'Dict' store.
data Handle m = Handle
  { hLoadValue :: !(Key -> m (Either DictErr (Maybe Value)))
  , hSaveValue :: !(Key -> Value -> m (Either DictErr ()))
  , hLoadDict :: !(Key -> m (Either DictErr Dict))
  , hSaveDict :: !(Key -> Dict -> m (Either DictErr ()))
  , hSaveDictPart :: !(Key -> Dict -> m (Either DictErr ()))
  , hLoadDictValue :: !(Key -> Key -> m (Either DictErr (Maybe Value)))
  , hLoadDictPart :: !(Key -> [Key] -> m (Either DictErr Dict))
  , hSaveDictValue :: !(Key -> Key -> Value -> m (Either DictErr ()))
  , hDeleteKeys :: !([Key] -> m (Either DictErr ()))
  , hDeleteMatchingKeys :: !(Key -> m (Either DictErr ()))
  , hDeleteDictKeys :: !(Key -> [Key] -> m (Either DictErr ()))
  , hLengthDict :: !(Key -> m (Either DictErr Natural))
  , hClose :: !(m ())
  }


-- | Represents the errors that might arise in 'Handle' functions.
data DictErr
  = ConnectionClosed
  | Unanticipated !Text
  | NotDecoded !Text
  | BadKey
  | Gone !Key
  deriving (Eq, Show)


instance Exception DictErr


-- | Represents a key used to store a 'Value'.
type Key = ByteString


-- | Represents a value stored in the service.
type Value = ByteString


-- | Represents a related group of @'Value'@s stored by @'Key'@.
type Dict = Map Key Value
