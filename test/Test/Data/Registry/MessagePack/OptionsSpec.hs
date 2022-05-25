{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.OptionsSpec where

import Data.Registry.MessagePack.Options
import Protolude
import Test.Tasty.Hedgehogx

test_qualifiers = test "qualifiers" $ do
  dropQualifier "x.y.z" === "z"
  dropQualifier "z" === "z"

  qualified "x.y.z" === "x.y.z"
  qualified "z" === "z"

  qualifyAs "a" "x.y.z" === "a.z"
  qualifyAs "a" "z" === "a.z"

  qualifyWithLastName "x.y.z" === "y.z"
  qualifyWithLastName "y.z" === "y.z"
  qualifyWithLastName "z" === "z"
