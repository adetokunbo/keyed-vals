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
  VarTest (VarTest),
  VarTestKey,
  VarTestID,
  FixedTest (FixedTest),
  FixedTestKey,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import KeyedVals.Handle.Codec.Aeson (ViaAeson (..))
import KeyedVals.Handle.Codec.HttpApiData (ViaHttpApiData (..))
import KeyedVals.Handle.Typed
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


{- | A simple type to demonstrate storing at variable paths.

it's just a simple type (Either) wrapped in newtype to avoid orphan
instances.
-}
newtype VarTest = VarTest (Either Text Bool)
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (Either Text Bool)
  deriving (DecodesFrom) via ViaAeson (Either Text Bool)
  deriving (EncodesAs) via ViaAeson (Either Text Bool)


-- | The keys for each 'VarTest' are @Int@s.
newtype VarTestKey = VarTestKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
  deriving (DecodesFrom) via ViaHttpApiData Int
  deriving (EncodesAs) via ViaHttpApiData Int


-- | Groups of 'VarTest' are stored for different 'VarTestID'.
newtype VarTestID = VarTestId Text
  deriving stock (Eq, Show)
  deriving (IsString, ToHttpApiData, FromHttpApiData) via Text
  deriving (DecodesFrom) via ViaHttpApiData Text
  deriving (EncodesAs) via ViaHttpApiData Text


-- | Describe how @'VarTest's@ are stored in the key-value store
instance PathOf VarTest where
  type KVPath VarTest = "/testing/{}/var"
  type KeyType VarTest = VarTestKey
  toKey _ = encodesAs


{- | Specify how to derive the path to store @'VarTest's@ in the key-value store

This instance uses 'substWebKey' to replace the @{}@ in the 'KVPath' with the
var.
-}
instance VaryingPathOf VarTest where
  type PathVar VarTest = VarTestID
  modifyPath _ = subst


{- | A simple type to demonstrate storing at a fixed path

it's just a simple type (tuple) wrapped in newtype to avoid orphan instances.
-}
newtype FixedTest = FixedTest (Int, Text)
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Int, Text)
  deriving (DecodesFrom) via ViaAeson (Int, Text)
  deriving (EncodesAs) via ViaAeson (Int, Text)


-- | Specify how @'FixedTest's@ are stored in the key-value store
instance PathOf FixedTest where
  type KVPath FixedTest = "/testing/fixed"
  type KeyType FixedTest = FixedTestKey
  toKey _ = encodesAs


-- | The keys for each 'FixedTest' are @Int@s.
newtype FixedTestKey = FixedTestKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
  deriving (DecodesFrom) via ViaHttpApiData Int
  deriving (EncodesAs) via ViaHttpApiData Int
