{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  TemplateHaskell functions
-}

module Data.Registry.MessagePack.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import Data.Registry.MessagePack.Options
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

-- | Remove the module name from a qualified name
makeName :: Options -> Name -> Name
makeName options = mkName . toS . modifyTypeName options . show

-- | Return the name of a given type with a modified name based on options
getSimpleTypeName :: Options -> Type -> Name
getSimpleTypeName options (ForallT _ _ ty) = getSimpleTypeName options ty
getSimpleTypeName options (VarT name) = makeName options name
getSimpleTypeName options (ConT name) = makeName options name
getSimpleTypeName options (TupleT n) = makeName options $ tupleTypeName n
getSimpleTypeName options ArrowT = makeName options ''(->)
getSimpleTypeName options ListT = makeName options ''[]
getSimpleTypeName options (AppT t1 t2) = mkName (show (getSimpleTypeName options t1) <> " " <> show (getSimpleTypeName options t2))
getSimpleTypeName options (SigT t _) = getSimpleTypeName options t
getSimpleTypeName options (UnboxedTupleT n) = makeName options $ unboxedTupleTypeName n
getSimpleTypeName _ t = panic $ "getSimpleTypeName: Unknown type: " <> show t
