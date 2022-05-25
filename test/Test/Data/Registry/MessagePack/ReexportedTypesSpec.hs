{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.ReexportedTypesSpec where

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Decoder
import Data.Registry.MessagePack.Encoder
import Data.Time
import qualified Data.Vector
import Protolude
import Test.Data.Registry.MessagePack.Reexported
import Prelude (String, fail)

{-

  In this specification we check that the code compiles
  when we produce non-qualified type names.
  They can then be found in a re-exported module

-}

encoders :: Registry _ _
encoders =
  $(makeEncoder ''Delivery)
    <: $(makeEncoder ''Person)
    <: $(makeEncoder ''Email)
    <: $(makeEncoder ''Identifier)
    <: $(makeEncoder ''BoxedValues)
    <: fun datetimeEncoder
    <: showEncoder @Integer
    <: encodeListOf @Text
    <: encodeMaybeOf @Int
    <: messagePackEncoder @Text
    <: messagePackEncoder @String
    <: messagePackEncoder @Int

datetimeEncoder :: Encoder DateTime
datetimeEncoder = Encoder (ObjectStr . toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" . _datetime)

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
    <: $(makeDecoder ''Person)
    <: $(makeDecoder ''Email)
    <: $(makeDecoder ''Identifier)
    <: $(makeDecoder ''BoxedValues)
    <: fun datetimeDecoder
    <: readDecoder @Integer
    <: decodeListOf @Text
    <: decodeMaybeOf @Int
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
