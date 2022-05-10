{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  An Encoder is used to encode a specific data type into a MessagePack Object
  This module provides several functions to create encoders and assemble them into a registry of encoders.
-}

module Data.Registry.MessagePack.Encoder where

import Control.Monad.Fail
import Data.Functor.Contravariant
import Data.List (nub)
import Data.MessagePack
import Data.Registry hiding (Result)
import Data.Registry.Internal.Types
import Data.Registry.MessagePack.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import Prelude (String)

-- * ENCODER DATA TYPE

newtype Encoder a = Encoder {encode :: a -> Object}

instance Contravariant Encoder where
  contramap f (Encoder a) = Encoder (a . f)

-- * ENCODE VALUES

encodeByteString :: Encoder a -> a -> ByteString
encodeByteString (Encoder e) = pack' . e

-- * CREATE ENCODERS

-- | Create an encoder from a MessagePack instance
messagePackEncoder :: forall a. (MessagePack a, Typeable a) => Typed (Encoder a)
messagePackEncoder = fun (messagePackEncoderOf @a)

messagePackEncoderOf :: MessagePack a => Encoder a
messagePackEncoderOf = Encoder toObject

-- | Create an encoder from a MessagePack instance
showEncoder :: forall a. (Typeable a, Show a) => Typed (Encoder String -> Encoder a)
showEncoder = fun showEncoderOf

showEncoderOf :: forall a. (Show a) => Encoder String -> Encoder a
showEncoderOf e = Encoder $ encode e . show

-- * COMBINATORS

-- | Create an Encoder for a (Maybe a)
encodeMaybeOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Maybe a))
encodeMaybeOf = fun (maybeOfEncoder @a)

maybeOfEncoder :: Encoder a -> Encoder (Maybe a)
maybeOfEncoder (Encoder e) = Encoder $ \case
  Nothing -> ObjectNil
  Just a -> e a

-- | Create an Encoder for a pair (a, b)
encodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Encoder a -> Encoder b -> Encoder (a, b))
encodePairOf = fun (pairOfEncoder @a @b)

pairOfEncoder :: Encoder a -> Encoder b -> Encoder (a, b)
pairOfEncoder (Encoder ea) (Encoder eb) =
  Encoder $ \(a, b) -> ObjectArray [ea a, eb b]

-- | Create an Encoder for a tripe (a, b, c)
encodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c))
encodeTripleOf = fun (tripleOfEncoder @a @b @c)

tripleOfEncoder :: Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c)
tripleOfEncoder (Encoder ea) (Encoder eb) (Encoder ec) =
  Encoder $ \(a, b, c) -> ObjectArray [ea a, eb b, ec c]

-- | Create an Encoder for a list [a]
encodeListOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder [a])
encodeListOf = fun (listOfEncoder @a)

listOfEncoder :: Encoder a -> Encoder [a]
listOfEncoder (Encoder ea) = Encoder $ \as -> toObject $ ea <$> as

-- | Create an Encoder for a non-empty list (NonEmpty a)
encodeNonEmptyOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (NonEmpty a))
encodeNonEmptyOf = fun (nonEmptyOfEncoder @a)

nonEmptyOfEncoder :: Encoder a -> Encoder (NonEmpty a)
nonEmptyOfEncoder (Encoder ea) = Encoder $ \as -> toObject $ ea <$> as

-- * TEMPLATE HASKELL

-- | Make an Encoder for a given data type
--   Usage: $(makeEncoder ''MyDataType <: otherEncoders)
makeEncoder :: Name -> ExpQ
makeEncoder encodedType = appE (varE $ mkName "fun") $ do
  info <- reify encodedType
  case info of
    -- \(ea::Encoder OldType) -> Encoder (\(NewType a) -> encode ea a)
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(_, _, other)]) _deriving) -> do
      lamE [sigP (varP $ mkName "ea") (appT (conT $ mkName "Encoder") (pure other))] (appE (conE $ mkName "Encoder") (lamE [conP constructor [varP $ mkName "a"]] (appE (appE (varE $ mkName "encode") (varE $ mkName "ea")) (varE $ mkName "a"))))
    -- \(e::Encoder OldType) -> Encoder (\(NewType a) -> encode e a)
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, other)]) _deriving) -> do
      lamE [sigP (varP $ mkName "ea") (appT (conT $ mkName "Encoder") (pure other))] (appE (conE $ mkName "Encoder") (lamE [conP constructor [varP $ mkName "a"]] (appE (appE (varE $ mkName "encode") (varE $ mkName "ea")) (varE $ mkName "a"))))
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [] -> do
          qReport True "can not make an Encoder for an empty data type"
          fail "encoders creation failed"
        [c] -> makeConstructorEncoder c
        _ -> makeConstructorsEncoder constructors
    other -> do
      qReport True ("can only create encoders for an ADT, got: " <> show other)
      fail "encoders creation failed"

-- | Make an Encoder for a data type with a single constructor
-- \(e0::Encoder A0) (e1::Encoder A1) ... -> Encoder $ \(T a0 a1 ...) -> ObjectArray [encode e0 a0, encode e1 a1, ...]
makeConstructorEncoder :: Con -> ExpQ
makeConstructorEncoder c = do
  ts <- typesOf c
  cName <- nameOf c
  let encoderParameters = (\(t, n) -> sigP (varP (mkName $ "e" <> show n)) (appT (conT $ mkName "Encoder") (pure t))) <$> zip ts [0 ..]
  let params = conP (mkName $ show cName) $ (\(_, n) -> varP (mkName $ "a" <> show n)) <$> zip ts [0 ..]
  let values = (\(_, n) -> appE (appE (varE $ mkName "encode") (varE (mkName $ "e" <> show n))) (varE (mkName $ "a" <> show n))) <$> zip ts [0 ..]
  let encoded = appE (conE (mkName "ObjectArray")) (appE (varE (mkName "Data.Vector.fromList")) (listE values))
  lamE encoderParameters (appE (conE (mkName "Encoder")) (lamE [params] encoded))

-- | Make an Encoder for a data type with several constructors
-- \(e0::Encoder A0) (e1::Encoder A1) (e2::Encoder A2) ... -> Encoder $ \case
--    T0  -> ObjectArray [ObjectInt 0]
--    T1 a0 a1 ... -> ObjectArray [ObjectInt 1, encode e0 a0, encode e1 a1, ...]
--    T2 a2 a0 ... -> ObjectArray [ObjectInt 2, encode e2 a2, encode e0 a0, ...]
makeConstructorsEncoder :: [Con] -> ExpQ
makeConstructorsEncoder cs = do
  -- get the types of all the fields of all the constructors
  ts <- nub . join <$> for cs typesOf
  let encoderParameters = (\(t, n) -> sigP (varP (mkName $ "e" <> show n)) (appT (conT $ mkName "Encoder") (pure t))) <$> zip ts [0 ..]
  matchClauses <- for (zip cs [0 ..]) (uncurry $ makeMatchClause ts)
  lamE encoderParameters (appE (conE (mkName "Encoder")) (lamCaseE (pure <$> matchClauses)))

-- | Make the match clause for a constructor given
--    - the list of all the encoder types
--    - the constructor name
--    - the constructor index in the list of all the constructors for the encoded data type
makeMatchClause :: [Type] -> Con -> Integer -> MatchQ
makeMatchClause allTypes c constructorIndex = do
  ts <- typesOf c
  constructorTypes <- indexConstructorTypes allTypes ts
  cName <- nameOf c
  let params = conP (mkName $ show cName) $ (\(_, n) -> varP (mkName $ "a" <> show n)) <$> constructorTypes
  let values = (\(_, n) -> appE (appE (varE $ mkName "encode") (varE (mkName $ "e" <> show n))) (varE (mkName $ "a" <> show n))) <$> constructorTypes
  let index = appE (conE $ mkName "ObjectInt") (litE (integerL constructorIndex))
  let encoded = appE (conE (mkName "ObjectArray")) (appE (varE (mkName "Data.Vector.fromList")) (listE $ index : values))
  match params (normalB encoded) []
