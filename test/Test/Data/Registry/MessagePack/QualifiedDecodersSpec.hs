{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.QualifiedDecodersSpec where

import Control.Monad.Fail
import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Decoder
import Data.Time
import Protolude
import Test.Data.Registry.MessagePack.DataTypes
import Prelude (String)

{-
  This specification checks that decoders built with qualified
  name will compile
-}

decoders :: Registry _ _
decoders =
  $(makeDecoderQualified ''Delivery)
    <: $(makeDecoderQualified ''Person)
    <: $(makeDecoderQualified ''Email)
    <: $(makeDecoderQualified ''Identifier)
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
