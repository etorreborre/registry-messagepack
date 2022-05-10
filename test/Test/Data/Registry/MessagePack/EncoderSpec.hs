{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.EncoderSpec where

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Encoder
import Data.Time
import Data.Vector
import Protolude
import Test.Data.Registry.MessagePack.DataTypes
import Test.Tasty.Hedgehogx
import Prelude (String)

test_encode = test "encode" $ do
  encode (make @(Encoder Delivery) encoders) delivery0 === ObjectArray [ObjectInt 0]
  encode (make @(Encoder Delivery) encoders) delivery1 === ObjectArray [ObjectInt 1, ObjectStr "me@here.com"]
  encode (make @(Encoder Delivery) encoders) delivery2 === ObjectArray [ObjectInt 2, ObjectArray [ObjectInt 123, ObjectStr "me@here.com"], ObjectStr "2022-04-18T00:00:12.000Z"]
  encode (make @(Encoder Integer) encoders) 10000000000000000 === ObjectStr "10000000000000000"

-- * HELPERS

encoders :: Registry _ _
encoders =
  $(makeEncoder ''Delivery)
    <: $(makeEncoder ''Person)
    <: $(makeEncoder ''Email)
    <: $(makeEncoder ''Identifier)
    <: fun datetimeEncoder
    <: showEncoder @Integer
    <: messagePackEncoder @Text
    <: messagePackEncoder @String
    <: messagePackEncoder @Int

datetimeEncoder :: Encoder DateTime
datetimeEncoder = Encoder (ObjectStr . toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" . _datetime)
