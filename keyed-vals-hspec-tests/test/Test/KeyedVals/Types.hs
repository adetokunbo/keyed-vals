{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.Type
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides types that demonstrate how to use @KeyVals.Handle.Typed@

The declared types are used in hspec tests used to validate implementations of 'Handle'
-}
module Test.KeyedVals.Types (
  -- * data types
  VarTest (..),
  VarTestKey (..),
  VarTestID,
  FixedTest (..),
  FixedTestKey (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import KeyedVals.Handle.Aeson
import KeyedVals.Handle.Typed
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


{- | A simple type to store with a variable path.

it's just a simple type (Either) wrapped in newtype to avoid orphan
instances.
-}
newtype VarTest = VarTest (Either Text Bool)
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (Either Text Bool)


-- | The keys for each 'VarTest' are @Int@s.
newtype VarTestKey = VarTestKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int


-- | Different groups of 'VarTest' are stored for different 'VarTestID'.
newtype VarTestID = VarTestId Text
  deriving stock (Eq, Show)
  deriving (IsString, ToHttpApiData, FromHttpApiData) via Text


-- | Specify how @'VarTest's@ are stored in the key-value store
instance PathOf VarTest where
  type KVPath VarTest = "/testing/{}/var"
  type KeyType VarTest = VarTestKey
  toKey _ = webKey


{- | Specify how to derive the path to store @'VarTest's@ in the key-value store

This instance uses 'substWebKey' to replace the @{}@ in the 'KVPath' with the
var.
-}
instance VaryingPathOf VarTest where
  type PathVar VarTest = VarTestID
  modifyPath _ = substWebKey


{- | A simple type to store with a fixed path

it's just a simple type (tuple) wrapped in newtype to avoid orphan instances.
-}
newtype FixedTest = FixedTest (Int, Text)
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Int, Text)


-- | Specify how @'FixedTest's@ are stored in the key-value store
instance PathOf FixedTest where
  type KVPath FixedTest = "/testing/fixed"
  type KeyType FixedTest = FixedTestKey
  toKey _ = webKey


-- | The keys for each 'FixedTest' are @Int@s.
newtype FixedTestKey = FixedTestKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int