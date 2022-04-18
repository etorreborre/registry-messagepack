module Test.Data.Registry.MessagePack.DataTypes where

import Data.Time
import Protolude

newtype Identifier = Identifier Int
  deriving (Eq, Show)

newtype Email = Email {_email :: Text}
  deriving (Eq, Show)

newtype DateTime = DateTime {_datetime :: UTCTime}
  deriving (Eq, Show)

data Person = Person {identifier :: Identifier, email :: Email}
  deriving (Eq, Show)

data Delivery
  = NoDelivery
  | ByEmail Email
  | InPerson Person DateTime
  deriving (Eq, Show)

data Path =
  File Int
  | Directory [Path]
  deriving (Eq, Show)

-- * EXAMPLES

email1 :: Email
email1 = Email "me@here.com"

person1 :: Person
person1 = Person (Identifier 123) email1

delivery0 :: Delivery
delivery0 = NoDelivery

delivery1 :: Delivery
delivery1 = ByEmail email1

delivery2 :: Delivery
delivery2 = InPerson person1 datetime1

datetime1 :: DateTime
datetime1 = DateTime $ UTCTime (fromGregorian 2022 4 18) 12

path1 :: Path
path1 = Directory [Directory [Directory [File 1], Directory [File 2]], File 3]
