module Data.Registry.MessagePack.Options where

import qualified Data.Text as T
import Protolude

-- | Options used to adjust the creation of encoders/decoders
newtype Options = Options
  { modifyTypeName :: Text -> Text
  }

-- | Default options for adjusting the creation of Encoders/Decoders
defaultOptions :: Options
defaultOptions = Options dropQualifier

-- | Drop the leading names in a qualified name
---  dropQualifier "x.y.z" === "z"
dropQualifier :: Text -> Text
dropQualifier t = fromMaybe t . lastMay $ T.splitOn "." t

-- | This function does not modify type names
qualified :: Text -> Text
qualified  = identity

-- | Provide a specific qualifier to produce type names
---  qualifyAs "a" "x.y.z" === "a.z"
qualifyAs :: Text -> Text -> Text
qualifyAs qualifier t = qualifier <> "." <> dropQualifier t

-- | Keep the last name as the qualifier
---  qualifyWithLastName "x.y.z" === "y.z"
qualifyWithLastName :: Text -> Text
qualifyWithLastName t =
  case reverse $ T.splitOn "." t of
    t1 : t2 : _ -> t2 <> "." <> t1
    _ -> t
