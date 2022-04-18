{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.MessagePack.RecursiveSpec where

import Data.MessagePack
import Data.Registry
import Data.Registry.MessagePack.Encoder
import Data.Vector
import Protolude
import Test.Data.Registry.MessagePack.DataTypes
import Test.Tasty.Hedgehogx

{-
  This module shows how to implement an encoder for a recursive data type.
  This could be supported by the makeEncoder function but it is not supported at the moment

-}
test_encode_recursive = test "encode a recursive data type" $ do
  encode (make @(Encoder Path) encoders) path1 === dir [dir [dir [file 1], dir [file 2]], file 3]

-- * HELPERS

dir :: [Object] -> Object
dir os = ObjectArray [ObjectInt 1, ObjectArray $ fromList os]

file :: Int -> Object
file n = ObjectArray [ObjectInt 0, toObject n]

encoders :: Registry _ _
encoders =
  fun pathEncoder
    <: messagePackEncoder @Int

pathEncoder :: Encoder Int -> Encoder Path
pathEncoder intEncoder = do
  let thisEncoder = Encoder $ \case
        File n -> ObjectArray [ObjectInt 0, encode intEncoder n]
        Directory paths -> ObjectArray [ObjectInt 1, ObjectArray (fromList $ encode thisEncoder <$> paths)]
  thisEncoder
