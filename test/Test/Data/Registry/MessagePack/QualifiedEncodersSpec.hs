{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.QualifiedEncodersSpec where

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Encoder
import Data.Time
import Data.Vector
import Protolude
import Test.Data.Registry.MessagePack.DataTypes (qualifiedAsOptions, qualifiedOptions, qualifiedWithLastOptions, BoxedValues (..))
import qualified Test.Data.Registry.MessagePack.DataTypes
import qualified Test.Data.Registry.MessagePack.DataTypes as DataTypes
import Prelude (String)

{-
  This specification checks that encoders built with qualified
  name will compile
-}

encoders :: Registry _ _
encoders =
  $(makeEncoderQualified ''Test.Data.Registry.MessagePack.DataTypes.Delivery)
    <: $(makeEncoderQualified ''Test.Data.Registry.MessagePack.DataTypes.Person)
    <: $(makeEncoderQualified ''Test.Data.Registry.MessagePack.DataTypes.Email)
    -- options can also be used directly
    <: $(makeEncoderWith qualifiedAsOptions ''Test.Data.Registry.MessagePack.DataTypes.Identifier)
    <: $(makeEncoderWith qualifiedOptions ''Test.Data.Registry.MessagePack.DataTypes.Identifier)
    <: $(makeEncoderWith qualifiedWithLastOptions ''Test.Data.Registry.MessagePack.DataTypes.Identifier)
    <: $(makeEncoderWith qualifiedOptions ''BoxedValues)
    <: fun datetimeEncoder
    <: showEncoder @Integer
    <: encodeListOf @Text
    <: encodeMaybeOf @Int
    <: messagePackEncoder @Text
    <: messagePackEncoder @String
    <: messagePackEncoder @Int

datetimeEncoder :: Encoder Test.Data.Registry.MessagePack.DataTypes.DateTime
datetimeEncoder = Encoder (ObjectStr . toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" . Test.Data.Registry.MessagePack.DataTypes._datetime)
