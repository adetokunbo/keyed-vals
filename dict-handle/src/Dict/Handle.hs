{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Declares the abstract @'Handle'@ and the functions used to access the dict's it provides.
-}
module Dict.Handle (
  -- * the 'Handle' type and its functions
  Handle (),
  loadValue,
  saveValue,
  loadDict,
  saveDict,
  saveDictPart,
  loadDictPart,
  loadDictValue,
  saveDictValue,
  deleteKeys,
  deleteMatchingKeys,
  deleteDictKeys,
  close,
  dictLength,

  -- * aliases used by the 'Handle' functions
  Key,
  Value,
  Dict,

  -- * error type for @Handle@ functions
  DictErr (..),
) where

import Dict.Handle.Internal
import Numeric.Natural (Natural)


-- | Loads the saved 'Value' corresponding to a 'Key'.
loadValue :: Handle m -> Key -> m (Either DictErr (Maybe Value))
loadValue = hLoadValue


-- | Saves a 'Value' corresponding to a 'Key'.
saveValue :: Handle m -> Key -> Value -> m (Either DictErr ())
saveValue = hSaveValue


-- | Loads a 'Dict'.
loadDict :: Handle m -> Key -> m (Either DictErr Dict)
loadDict = hLoadDict


-- | Saves a 'Dict' .
saveDict :: Handle m -> Key -> Dict -> m (Either DictErr ())
saveDict = hSaveDict


-- | Loads a @'Value'@ from a 'Dict'.
loadDictValue :: Handle m -> Key -> Key -> m (Either DictErr (Maybe Value))
loadDictValue = hLoadDictValue


-- | Saves a @'Value'@ in a 'Dict'.
saveDictValue :: Handle m -> Key -> Key -> Value -> m (Either DictErr ())
saveDictValue = hSaveDictValue


-- | Loads a 'Dict' of the @'Value's@ found for the given @'Key's@.
loadDictPart :: Handle m -> Key -> [Key] -> m (Either DictErr Dict)
loadDictPart = hLoadDictPart


-- | Updates the stored 'Dict' with the @Value's@ from the given 'Dict'.
saveDictPart :: Handle m -> Key -> Dict -> m (Either DictErr ())
saveDictPart = hSaveDictPart


-- | Deletes @'Value's@ for the given @'Key's@.
deleteKeys :: Handle m -> [Key] -> m (Either DictErr ())
deleteKeys = hDeleteKeys


-- | Deletes values whose keys match the given pattern.
deleteMatchingKeys :: Handle m -> Key -> m (Either DictErr ())
deleteMatchingKeys = hDeleteMatchingKeys


-- | Deletes @'Value's@ for the given @'Key's@ from a 'Dict'.
deleteDictKeys :: Handle m -> Key -> [Key] -> m (Either DictErr ())
deleteDictKeys = hDeleteDictKeys


-- | Determines the number of @'Values'@ in a 'Dict'.
dictLength :: Handle m -> Key -> m (Either DictErr Natural)
dictLength = hLengthDict


close :: Handle m -> m ()
close = hClose
