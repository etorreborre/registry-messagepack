{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  TemplateHaskell functions
-}

module Data.Registry.MessagePack.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

indexConstructorTypes :: [Type] -> [Type] -> Q [(Type, Int)]
indexConstructorTypes allTypes constructorTypes =
  for constructorTypes $ \t ->
    case elemIndex t allTypes of
      Just n -> pure (t, n)
      Nothing -> fail $ "the type " <> show t <> " cannot be found in the list of all types " <> show allTypes

-- | Get the types of all the fields of a constructor
typesOf :: Con -> Q [Type]
typesOf (NormalC _ types) = pure (snd <$> types)
typesOf (RecC _ types) = pure $ (\(_, _, t) -> t) <$> types
typesOf other = do
  qReport True ("we can only create encoders for normal constructors and records, got: " <> show other)
  fail "encoders creation failed"

nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create encoders for normal constructors and records, got: " <> show other)
  fail "encoders creation failed"
