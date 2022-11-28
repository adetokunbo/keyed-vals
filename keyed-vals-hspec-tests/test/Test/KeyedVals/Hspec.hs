{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.Hspec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.KeyedVals.Hspec (
  -- * a test fixture
  checkHandle,

  -- * functions for extending the fixture
  orThrowHandleErr,
  throwHandleErr,
  withGlobOf,

  -- * setup/teardown hspec te+l fssts
  setupFixture,
  closeFixture,

  -- * module re-eports
  module Test.Hspec,
  module Test.Hspec.Benri,
) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import KeyedVals.Handle
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Benri


checkHandle :: SpecWith (Handle IO)
checkHandle = do
  context "with simple values" $ do
    it "should load ok" $ \h -> do
      loadVal h key1 `endsRight` Just simple1

    it "should update ok" $ \h -> do
      endsRight_ $ saveVal h key1 "changed"
      loadVal h key1 `endsRight` Just "changed"

    it "should delete matching keys correctly " $ \h -> do
      loadVal h key2 `endsRight` Just simple2
      withGlobOf "*2" $ \patt -> do
        endsRight_ $ deleteMatches h patt
        loadVal h key2 `endsRight` Nothing

    it "should delete ok" $ \h -> do
      deleteKeys h (key1 :| []) `endsRight` ()
      loadVal h key1 `endsRight` Nothing

  context "with dict values" $ do
    let mKey1Of h = loadFrom h mKey1

    it "should load ok" $ \h -> do
      loadKVs h mKey1 `endsRight` d1

    checkLength mKey1 4

    it "should update an indexed value ok" $ \h -> do
      endsRight_ $ saveTo h mKey1 key1 "changed"
      mKey1Of h key1 `endsRight` Just "changed"

    checkLength mKey1 4

    it "should add an indexed value ok" $ \h -> do
      mKey1Of h "foo" `endsRight` Nothing
      endsRight_ $ saveTo h mKey1 "foo" "bar"
      mKey1Of h "foo" `endsRight` Just "bar"

    checkLength mKey1 5

    it "should delete indexed values ok" $ \h -> do
      endsRight_ $ deleteKeysFrom h mKey1 (key1 :| [key2])
      mKey1Of h "foo" `endsRight` Just "bar"
      mKey1Of h key1 `endsRight` Nothing
      mKey1Of h key2 `endsRight` Nothing

    checkLength mKey1 3

    it "should update the indexed values using a dict ok" $ \h -> do
      endsRight_ $ updateKVs h mKey1 d2
      mKey1Of h key1 `endsRight` Just simple3
      mKey1Of h key2 `endsRight` Nothing
      mKey1Of h key5 `endsRight` Just simple3

    it "should fetch a subset of the indexed values as a dict ok" $ \h -> do
      let want = Map.fromList [("foo", "bar"), (key5, simple3)]
      loadSlice h mKey1 (AllOf ("foo" :| [key5])) `endsRight` want

    it "should delete ok" $ \h -> do
      endsRight_ $ deleteKeys h (mKey1 :| [])
      loadKVs h mKey1 `endsRight` Map.empty
      mKey1Of h "foo" `endsRight` Nothing

    checkLength mKey1 0


withGlobOf :: ByteString -> (Glob -> IO a) -> IO a
withGlobOf x action = do
  case mkGlob x of
    Nothing -> throwIO $ userError "bad test pattern"
    Just g -> action g


checkLength :: Key -> Natural -> SpecWith (Handle IO)
checkLength aKey n = context "and the reported length" $ do
  it "should be correct " $ \h -> do
    countKVs h aKey `endsRight` n


setupFixture :: Handle IO -> IO (Handle IO)
setupFixture h = do
  orThrowHandleErr $ saveVal h key1 simple1
  orThrowHandleErr $ saveVal h key2 simple2
  orThrowHandleErr $ saveKVs h mKey1 d1
  pure h


closeFixture :: Handle IO -> IO ()
closeFixture = close


key1, key2, key3, key4, key5, mKey1 :: Key
key1 = "a-simple-key-1"
key2 = "a-simple-key-2"
key3 = "another-key-1"
key4 = "another_key-2"
key5 = "yet-another-key"
mKey1 = "a-map-key-1"


simple1, simple2, simple3 :: Val
simple1 = "a-simple-value-1"
simple2 = "a-simple-value-2"
simple3 = "a-simple-value-3"


d1, d2 :: ValsByKey
d1 = Map.fromList [(key1, simple1), (key2, simple2), (key3, simple1), (key4, simple2)]
d2 = Map.fromList [(key1, simple3), (key5, simple3)]


throwHandleErr :: HandleErr -> IO ()
throwHandleErr = throwIO . userError . show


orThrowHandleErr :: IO (Either HandleErr ()) -> IO ()
orThrowHandleErr action = action >>= either throwHandleErr pure
