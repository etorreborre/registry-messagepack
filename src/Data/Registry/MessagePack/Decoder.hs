{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  A Decoder is used to decode a MessagePack Object into a specific data type
  This module provides several functions to create decoders and assemble them into a registry of encoders.
-}
module Data.Registry.MessagePack.Decoder where

import Control.Monad.Fail
import Data.List (nub)
import Data.MessagePack as MP
import Data.Registry
import Data.Registry.Internal.Types
import Data.Registry.MessagePack.Options
import Data.Registry.MessagePack.TH
import qualified Data.Vector as Vector
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import Prelude (String)

-- * DECODER DATA TYPE

newtype Decoder a = Decoder {decode :: Object -> MP.Result a}

instance Functor Decoder where
  fmap f (Decoder d) = Decoder (fmap f . d)

instance Applicative Decoder where
  pure a = Decoder (const (pure a))
  f <*> a = uncurry ($) <$> decoderAp f a

decoderAp :: Decoder a -> Decoder b -> Decoder (a, b)
decoderAp (Decoder da) (Decoder db) = Decoder $ \case
  o@(ObjectArray ls) ->
    case reverse (toList ls) of
      b : as -> (,) <$> da (ObjectArray $ Vector.fromList $ reverse as) <*> db b
      [] -> (,) <$> da o <*> db o
  o -> (,) <$> da o <*> db o

-- * DECODING

-- | Use a Decoder to decode a ByteString into the desired type
decodeByteString :: forall a. (Typeable a) => Decoder a -> ByteString -> Either Text a
decodeByteString d bs =
  case unpack' bs of
    Left e -> Left $ "cannot unpack the bytestring as an Object: " <> show e <> ". The bytestring is: " <> show bs
    Right o ->
      case decodeObject d o of
        Right a -> pure a
        Left e -> Left $ "Error: " <> toS e <> ". Cannot decode " <> toS (showType @a) <> " from the Object: " <> show o

-- | Use a Decoder to decode an Object into the desired type
decodeObject :: Decoder a -> Object -> Either Text a
decodeObject (Decoder d) object =
  case d object of
    Success a -> Right a
    Error e -> Left (toS e)

-- * CREATING DECODERS

-- | Add a Decoder a to a registry of decoders when a MessagePack a instance exists
--   usage: decoders = messagePackDecoder @a <: otherDecoders
messagePackDecoder :: forall a. (MessagePack a, Typeable a) => Typed (Decoder a)
messagePackDecoder = fun (messagePackDecoderOf @a)

messagePackDecoderOf :: MessagePack a => Decoder a
messagePackDecoderOf = Decoder fromObject

-- | Create a Decoder from a Read instance
readDecoder :: forall a. (Typeable a, Read a) => Typed (Decoder String -> Decoder a)
readDecoder = fun readDecoderOf

readDecoderOf :: forall a. (Read a) => Decoder String -> Decoder a
readDecoderOf ds = Decoder $ decode ds >=> (either Error Success . readEither)

-- * COMBINATORS

-- | Add a Maybe (Decoder a) to a registry of decoders
--   usage: decoders = decodeMaybeOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeMaybeOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder (Maybe a))
decodeMaybeOf = fun (maybeOfDecoder @a)

maybeOfDecoder :: forall a. Decoder a -> Decoder (Maybe a)
maybeOfDecoder (Decoder d) = Decoder $ \case
  ObjectNil -> pure Nothing
  just -> Just <$> d just

-- | Add a Maybe (a, b) to a registry of decoders
--   usage: decoders = decodePairOf @a @b <: otherDecoders
--   the list of otherDecoders must contain a Decoder a and a Decoder b
--   otherwise there will be a compilation error
decodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Decoder a -> Decoder b -> Decoder (a, b))
decodePairOf = fun (pairOfDecoder @a @b)

pairOfDecoder :: forall a b. (Typeable a, Typeable b) => Decoder a -> Decoder b -> Decoder (a, b)
pairOfDecoder (Decoder a) (Decoder b) = Decoder $ \case
  ObjectArray [oa, ob] -> (,) <$> a oa <*> b ob
  other -> Error $ "not a pair of " <> showType @a <> "," <> showType @b <> ": " <> show other

-- | Add a Maybe (a, b, c) to a registry of decoders
--   usage: decoders = decodeTripleOf @a @b @c <: otherDecoders
--   the list of otherDecoders must contain a Decoder a, a Decoder b and a Decoder c
--   otherwise there will be a compilation error
decodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c))
decodeTripleOf = fun (tripleOfDecoder @a @b @c)

tripleOfDecoder :: forall a b c. (Typeable a, Typeable b, Typeable c) => Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
tripleOfDecoder (Decoder a) (Decoder b) (Decoder c) = Decoder $ \case
  ObjectArray [oa, ob, oc] -> (,,) <$> a oa <*> b ob <*> c oc
  other -> Error $ "not a triple of " <> showType @a <> "," <> showType @b <> "," <> showType @c <> ": " <> show other

-- | Add a Decoder [a] to a registry of decoders
--   usage: decoders = decodeListOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeListOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder [a])
decodeListOf = fun (listOfDecoder @a)

listOfDecoder :: forall a. (Typeable a) => Decoder a -> Decoder [a]
listOfDecoder (Decoder a) = Decoder $ \case
  ObjectArray os -> for (toList os) a
  other -> Error $ "not a list of " <> showType @a <> ": " <> show other

-- | Add a Decoder (NonEmpty a) to a registry of decoders
--   usage: decoders = decodeNonEmptyOf @a <: otherDecoders
--   the list of otherDecoders must contain a Decoder a
--   otherwise there will be a compilation error
decodeNonEmptyOf :: forall a. (Typeable a) => Typed (Decoder a -> Decoder (NonEmpty a))
decodeNonEmptyOf = fun (nonEmptyOfDecoder @a)

nonEmptyOfDecoder :: forall a. (Typeable a) => Decoder a -> Decoder (NonEmpty a)
nonEmptyOfDecoder (Decoder a) = Decoder $ \case
  ObjectArray values ->
    case toList values of
      [] -> Error $ "expected a NonEmpty of " <> showType @a
      o : os -> (:|) <$> a o <*> for os a
  other -> Error $ "not a list of " <> showType @a <> ": " <> show other

showType :: forall a. (Typeable a) => String
showType = show (typeRep (Proxy :: Proxy a))

-- * TEMPLATE HASKELL

-- | Make a Decoder for a given data type
--   Usage: $(makeDecoder ''MyDataType <: otherDecoders)
makeDecoder :: Name -> ExpQ
makeDecoder = makeDecoderWith defaultOptions

-- | Make a Decoder for a given data type, where all constructors and decoded types are qualified
--   Usage: $(makeDecoderQualified ''MyDataType <: otherDecoders)
makeDecoderQualified :: Name -> ExpQ
makeDecoderQualified = makeDecoderWith (Options identity)

-- | Make a Decoder for a given data type, where all constructors and decoded types are qualified
--   Usage: $(makeDecoderQualifiedLast ''MyDataType <: otherDecoders)
makeDecoderQualifiedLast :: Name -> ExpQ
makeDecoderQualifiedLast = makeDecoderWith (Options qualifyWithLastName)

-- | Make a Decoder with a given set of options
--   Usage: $(makeDecoderWith (Options qualify) ''MyDataType <: otherDecoders)
makeDecoderWith :: Options -> Name -> ExpQ
makeDecoderWith options typeName = appE (varE $ mkName "fun") $ do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(_, _, other)]) _deriving) -> do
      -- \(a::Decoder OldType) -> fmap NewType d
      let cName = mkName $ show $ makeName options constructor
      lamE [sigP (varP $ mkName "d") (appT (conT $ mkName "Decoder") (pure other))] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "d"))
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, other)]) _deriving) -> do
      -- \(a::Decoder OldType) -> fmap NewType d
      let cName = mkName $ show $ makeName options constructor
      lamE [sigP (varP $ mkName "d") (appT (conT $ mkName "Decoder") (pure other))] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "d"))
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [] -> do
          qReport True "can not make an Decoder for an empty data type"
          fail "decoders creation failed"
        [c] -> makeConstructorDecoder options typeName c
        _ -> makeConstructorsDecoder options typeName constructors
    other -> do
      qReport True ("can only create decoders for an ADT, got: " <> show other)
      fail "decoders creation failed"

-- | Make a Decoder for a single Constructor, where each field of the constructor is encoded as an element of an ObjectArray
makeConstructorDecoder :: Options -> Name -> Con -> ExpQ
makeConstructorDecoder options typeName c = do
  ts <- typesOf c
  cName <- makeName options <$> nameOf c
  let decoderParameters = (\(t, n) -> sigP (varP (mkName $ "d" <> show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..]
  let paramP = varP (mkName "o")
  let paramE = varE (mkName "o")
  let paramsP = (\n -> varP $ mkName $ "o" <> show n) <$> [0 .. length ts -1]
  let matchClause =
        match
          (conP (mkName "ObjectArray") [viewP (varE (mkName "toList")) (listP paramsP)])
          (normalB (applyDecoder cName [0 .. length ts - 1]))
          []

  let decoded = caseE paramE [matchClause, makeErrorClause options typeName]

  -- (\(d1::Decoder Type1) (d2::Decoder Type2) ... -> Decoder (\case
  --     ObjectArray (toList -> [o1, o2, ...]) -> Constructor <$> decode d1 o1 <*> decode d2 o2 ...))
  --     other -> Error ("not a valid " <> constructorType <> ": " <> show other)
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] decoded))

-- | Make a Decoder for a each Constructor of a data type:
--     - each constructor is specified by an ObjectArray [ObjectInt n, o1, o2, ...]
--     - n specifies the number of the constructor
--     - each object in the array represents a constructor field
makeConstructorsDecoder :: Options -> Name -> [Con] -> ExpQ
makeConstructorsDecoder options typeName cs = do
  ts <- nub . join <$> for cs typesOf
  let decoderParameters = (\(t, n) -> sigP (varP (mkName $ "d" <> show n)) (appT (conT $ mkName "Decoder") (pure t))) <$> zip ts [0 ..]
  let paramP = varP (mkName "o")
  let paramE = varE (mkName "o")
  let matchClauses = uncurry (makeMatchClause options ts) <$> zip cs [0 ..]
  let errorClause = makeErrorClause options typeName
  let decoded = caseE paramE (matchClauses <> [errorClause])

  -- (\(d1::Decoder Type1) (d2::Decoder Type2) ... -> Decoder (\case
  --     ObjectArray (toList -> [ObjectInt n, o1, o2, ...]) -> Constructor <$> decode d1 o1 <*> decode d2 o2 ...))
  --     other -> Error ("not a valid " <> constructorType <> ": " <> show other)
  lamE decoderParameters (appE (conE (mkName "Decoder")) (lamE [paramP] decoded))

-- | Return an error if an object is not an ObjectArray as expected
--   other -> Error (mconcat ["not a valid ", show typeName, ": ", show other])
makeErrorClause :: Options -> Name -> MatchQ
makeErrorClause options typeName = do
  let errorMessage =
        appE (varE $ mkName "mconcat") $
          listE
            [ litE (StringL "not a valid "),
              litE (StringL . show $ makeName options typeName),
              litE (StringL ": "),
              appE (varE $ mkName "show") (varE $ mkName "_1")
            ]
  match (varP $ mkName "_1") (normalB (appE (conE $ mkName "Error") errorMessage)) []

-- | Decode the nth constructor of a data type
makeMatchClause :: Options -> [Type] -> Con -> Integer -> MatchQ
makeMatchClause options allTypes c constructorIndex = do
  ts <- typesOf c
  constructorTypes <- fmap snd <$> indexConstructorTypes allTypes ts
  cName <- makeName options <$> nameOf c
  let paramsP = conP (mkName "ObjectInt") [litP (IntegerL constructorIndex)] : ((\n -> varP $ mkName $ "o" <> show n) <$> constructorTypes)
  match
    (conP (mkName "ObjectArray") [viewP (varE (mkName "toList")) (listP paramsP)])
    (normalB (applyDecoder cName constructorTypes))
    []

-- ConstructorName <$> decode d1 o1 <*> decode d2 o2 ...
applyDecoder :: Name -> [Int] -> ExpQ
applyDecoder cName [] = appE (varE $ mkName "pure") (conE cName)
applyDecoder cName (n : ns) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\i r -> appE (appE (varE (mkName "ap")) r) $ decodeAt i) (appE (appE (varE (mkName "ap")) cons) $ decodeAt n) (reverse ns)
  where
    decodeAt i = appE (appE (varE $ mkName "decode") (varE $ mkName ("d" <> show i))) (varE $ mkName $ "o" <> show i)
