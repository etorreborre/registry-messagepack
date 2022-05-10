{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.DecoderSpec where

import Control.Monad.Fail
import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Decoder
import Data.Time
import Protolude
import Test.Data.Registry.MessagePack.DataTypes
import Test.Tasty.Hedgehogx
import Prelude (String)

test_decode = test "decode" $ do
  decode (make @(Decoder Delivery) decoders) (ObjectArray [ObjectInt 0]) === Success delivery0
  decode (make @(Decoder Delivery) decoders) (ObjectArray [ObjectInt 1, ObjectStr "me@here.com"]) === Success delivery1
  decode (make @(Decoder Delivery) decoders) (ObjectArray [ObjectInt 2, ObjectArray [ObjectInt 123, ObjectStr "me@here.com"], ObjectStr "2022-04-18T00:00:12.000Z"]) === Success delivery2
  decode (make @(Decoder Integer) decoders) (ObjectStr "10000000000000000") === Success 10000000000000000

-- * HELPERS

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
    <: $(makeDecoder ''Person)
    <: $(makeDecoder ''Email)
    <: $(makeDecoder ''Identifier)
    <: fun datetimeDecoder
    <: readDecoder @Integer
    <: messagePackDecoder @Text
    <: messagePackDecoder @String
    <: messagePackDecoder @Int

datetimeDecoder :: Decoder DateTime
datetimeDecoder = Decoder $ \case
  ObjectStr s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure (DateTime t)
      Nothing -> fail ("cannot read a DateTime: " <> toS s)
  other -> Error $ "not a valid DateTime: " <> show other
