{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.CheckHandle
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetcorrectlyunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.KeyedVals.CheckTypedHandle (
  -- * a test fixture
  spec,

  -- * setup/teardown hspec tests
  setupFixture,
  closeFixture,
) where

import qualified Data.Map.Strict as Map
import KeyedVals.Handle.Typed
import Test.KeyedVals.Prelude


spec :: SpecWith (Handle IO)
spec = do
  checkFixedPathed
  checkVarPathed


checkFixedPathed :: SpecWith (Handle IO)
checkFixedPathed = do
  context "with typed key-values stored in fixed paths" $ do
    it "should load correctly" $ \h -> do
      loadKVs h Fixed `endsRight` fixedKVs

    checkLength @FixedTest Fixed 2

    it "should update an indexed value correctly" $ \h -> do
      let key = Raw fixedK1
          want = FixedTest (1, "changed")
      endsRight_ $ saveTo h key want
      loadFrom h key `endsRight` want
      mayLoadFrom h key `endsRight` Just want

    checkLength @FixedTest Fixed 2

    it "should add an indexed value correctly" $ \h -> do
      let added = Raw fixedK3
          want = FixedTest (3, "added")
      mayLoadFrom h added `endsRight` Nothing
      endsRight_ $ saveTo h added want
      mayLoadFrom h added `endsRight` Just want

    checkLength @FixedTest Fixed 3

    it "should update the key-values correctly" $ \h -> do
      endsRight_ $ updateKVs h Fixed moreFixedKVs
      mayLoadFrom h (Raw fixedK1) `endsRight` Just fixedV3
      mayLoadFrom h (Raw fixedK4) `endsRight` Just fixedV4

    it "should fetch a subset of the key-values as a dict correctly" $ \h -> do
      let selection = fixedK1 :| [fixedK4]
      loadSlice h Fixed selection `endsRight` moreFixedKVs


checkVarPathed :: SpecWith (Handle IO)
checkVarPathed = do
  context "with typed key-values stored in variable paths" $ do
    it "should load correctly" $ \h -> do
      loadKVs h path1 `endsRight` varKVs
      loadKVs h path2 `endsRight` varKVs

    checkLength path1 2
    checkLength path2 2

    it "should update an indexed value correctly" $ \h -> do
      let key = Extended id1 varK1
          want = VarTest $ Right False
      endsRight_ $ saveTo h key want
      loadFrom h key `endsRight` want
      mayLoadFrom h key `endsRight` Just want

    it "should add an indexed value correctly" $ \h -> do
      let added = Extended id2 varK3
          want = VarTest $ Left "added"
      mayLoadFrom h added `endsRight` Nothing
      endsRight_ $ saveTo h added want
      mayLoadFrom h added `endsRight` Just want

    checkLength path1 2
    checkLength path2 3

    it "should update the key-values correctly" $ \h -> do
      endsRight_ $ updateKVs h path1 moreVarKVs
      mayLoadFrom h (Extended id1 varK1) `endsRight` Just varV3
      mayLoadFrom h (Extended id1 varK4) `endsRight` Just varV4

    it "should fetch a subset of the key-values correctly" $ \h -> do
      let selection = varK1 :| [varK4]
      loadSlice h path1 selection `endsRight` moreVarKVs


setupFixture :: Handle IO -> IO (Handle IO)
setupFixture h = do
  orThrowHandleErr $ saveKVs h Fixed fixedKVs
  orThrowHandleErr $ saveKVs h path1 varKVs
  orThrowHandleErr $ saveKVs h path2 varKVs
  pure h


closeFixture :: Handle IO -> IO ()
closeFixture = close


fixedK1, fixedK2, fixedK3, fixedK4 :: FixedTestKey
fixedK1 = 25
fixedK2 = 49
fixedK3 = 81
fixedK4 = 121


fixedV1, fixedV2, fixedV3, fixedV4 :: FixedTest
fixedV1 = FixedTest (1, "one")
fixedV2 = FixedTest (2, "two")
fixedV3 = FixedTest (1, "un")
fixedV4 = FixedTest (4, "quatre")


fixedKVs, moreFixedKVs :: TypedKVs FixedTest
fixedKVs =
  Map.fromList
    [ (fixedK1, fixedV1)
    , (fixedK2, fixedV2)
    ]
moreFixedKVs =
  Map.fromList
    [ (fixedK1, fixedV3)
    , (fixedK4, fixedV4)
    ]


varK1, varK2, varK3, varK4 :: VarTestKey
varK1 = 36
varK2 = 64
varK3 = 100
varK4 = 144


path1, path2 :: TypedPath VarTest
path1 = Variable id1
path2 = Variable id2


id1, id2 :: VarTestID
id1 = "id1"
id2 = "id2"


varV1, varV2, varV3, varV4 :: VarTest
varV1 = VarTest $ Left "one"
varV2 = VarTest $ Right False
varV3 = VarTest $ Left "three"
varV4 = VarTest $ Left "four"


varKVs, moreVarKVs :: TypedKVs VarTest
varKVs =
  Map.fromList
    [ (varK1, varV1)
    , (varK2, varV2)
    ]
moreVarKVs =
  Map.fromList
    [ (varK1, varV3)
    , (varK4, varV4)
    ]


checkLength ::
  (Ord (KeyType v)) =>
  TypedPath v ->
  Natural ->
  SpecWith (Handle IO)
checkLength path n = context "and the reported size" $ do
  it "should be correct " $ \h -> do
    countKVs h path `endsRight` n
