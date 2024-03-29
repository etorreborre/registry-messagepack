# `registry-messagepack`

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), providing customizable encoders / decoders for [MessagePack](https://msgpack.org/).
The approach taken is to add to a registry a list of functions taking encoders / decoders as parameters and producing encoders / decoders.
Then `registry` is able to assemble all the functions required to make an `Encoder` or a `Decoder` of a given type if the encoders or decoders for its dependencies can
be made out of the registry.

### Encoders

#### Example

Here is an example of creating encoders for a set of related data types:
```haskell
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Encoder
import Data.Time
import Data.Vector
import Protolude

newtype Identifier = Identifier Int
newtype Email = Email { _email :: Text }
newtype DateTime = DateTime { _datetime :: UTCTime }
data Person = Person { identifier :: Identifier, email :: Email }

data Delivery =
    NoDelivery
  | ByEmail Email
  | InPerson Person DateTime

encoders :: Registry _ _
encoders =
  $(makeEncoder ''Delivery)
  <: $(makeEncoder ''Person)
  <: $(makeEncoder ''Email)
  <: $(makeEncoder ''Identifier)
  <: fun datetimeEncoder
  <: messagePackEncoder @Text
  <: messagePackEncoder @Int

datetimeEncoder :: Encoder DateTime
datetimeEncoder = Encoder (ObjectStr . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" . _datetime)
```

In the code above most encoders are created with `TemplateHaskell` and the `makeEncoder` function. The other encoders are either:

 - created manually: `dateTimeEncoder` (note that this encoder needs to be added to the registry with `fun`)
 - retrieved from a `MessagePack` instance: `messagePackEncoder @Text`, `messagePackEncoder @Int`

Given the list of `encoders` an `Encoder Person` can be retrieved with:
```haskell
let encoderPerson = make @(Encoder Person) encoders
let encoded = encode encoderPerson (Person (Identifier 123) (Email "me@here.com")) :: Object
```

#### Generated encoders

The `makeEncoder` function makes the following functions:
```haskell
-- makeEncoder ''Identifier
\(e::Encoder Int) -> Encoder $ \(Identifier n) -> encode e n

-- makeEncoder ''Email
\(e::Encoder Text) -> Encoder $ \(Email s) -> encode e s

-- makeEncoder ''Person
\(e1::Encoder Identifier) (e2::Encoder Email) -> Encoder $ \(Person a1 a2) -> ObjectArray [encode e1 a1, encode e2 a2]

-- makeEncoder ''Delivery
\(e1::Encoder Email) (e2::Encoder Person) (e3::Encoder DateTime) -> Encoder $ \case
  NoDelivery -> ObjectArray [ObjectInt 0]
  ByEmail a1 -> ObjectArray [ObjectInt 1, encode e1 a1]
  InPerson a1 a2 -> ObjectArray [ObjectInt 2, encode e1 a1, encode e2 a2]
```

__NOTE__ this function does not support recursive data types (and much less mutually recursive data types)

### Decoders

#### Example

Here is an example of creating decoders for a set of related data types:
```haskell
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Decoder
import Data.Time
import Protolude

newtype Identifier = Identifier Int
newtype Email = Email { _email :: Text }
newtype DateTime = DateTime { _datetime :: UTCTime }
data Person = Person { identifier :: Identifier, email :: Email }

data Delivery =
    NoDelivery
  | ByEmail Email
  | InPerson Person DateTime

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
  <: $(makeDecoder ''Person)
  <: $(makeDecoder ''Email)
  <: $(makeDecoder ''Identifier)
  <: fun dateTimeDecoder
  <: messagePackDecoder @Text
  <: messagePackDecoder @Int

dateTimeDecoder :: Decoder DateTime
dateTimeDecoder = Decoder $ \case
  ObjectStr s -> DateTime <$> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s
  other -> Error ("not a valid DateTime: " <> show other)
```

In the code above most decoders are created with `TemplateHaskell` and the `makeDecoder` function. The other decoders are either:

 - created manually: `dateTimeDecoder` (note that this decoder needs to be added to the registry with `fun`)
 - retrieved from a `MessagePack` instance: `messagePackDecoder @Text`, `messagePackDecoder @Int`

Given the list of `Decoders` an `Decoder Person` can be retrieved with:
```haskell
let decoderPerson = make @(Decoder Person) decoders
let decoded = decode decoderPerson $ ObjectArray [ObjectInt 123, ObjectStr "me@here.com"]
```

#### Generated decoders

The `makeDecoder` function makes the following functions:
```haskell
-- makeDecoder ''Identifier
\(d::Decoder Int) -> Decoder $ \o -> Identifier <$> decode d o

-- makeDecoder ''Email
\(d::Decoder Text) -> Decoder $ \o -> Email <$> decode d o

-- makeDecoder ''Person
\(d1::Decoder Identifier) (d2::Decoder Email) -> Decoder $ \case
  ObjectArray [o1, o2] -> Person <$> decode d1 o1 <*> decode d2 o2
  other -> Error ("not a valid Person: " <> show other)

-- makeDecoder ''Delivery
\(d1::Decoder Email) (d2::Decoder Person) (d3::Decoder DateTime) -> Decoder $ \case
  ObjectArray [ObjectInt 0] -> pure NoDelivery
  ObjectArray [ObjectInt 1, o1] -> ByEmail <$> decode d1 o1
  ObjectArray [ObjectInt 2, o1, o2] -> InPerson <$> decode d1 o1 <*> decode d2 o2
  other -> Error ("not a valid Delivery: " <> show other)
```

__NOTE__ this function does not support recursive data types (and much less mutually recursive data types)

### Generation options

When using TemplateHaskell to generate encoders and decoders, all type names will be generated as unqualified by default.
You can instead opt for qualified names by specifying an `Options` value:
```haskell
import Data.Registry.MessagePack.Encoder
import Data.Registry.MessagePack.Options

-- DataTypes defines the Email and Person data types
import qualified DataTypes as DT
import qualified Data.Common

encoders =
  <: $(makeEncoderWith defaultOptions {modifyTypeName = qualified} ''DT.Person)
  <: $(makeEncoderWith defaultOptions {modifyTypeName = qualifiedAs "DT"} ''DT.Email)
  <: $(makeEncoderWith defaultOptions {modifyTypeName = qualifiedWithLastName} ''Common.Age)
```

__NOTE__ only the data type and its constructors are qualified when you use `qualifiedXXX` functions
