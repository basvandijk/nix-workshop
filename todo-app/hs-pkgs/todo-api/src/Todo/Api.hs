{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Api where

import Servant.API
import Control.Lens (makeLenses)
import qualified Data.Text as T
import Data.Map (Map)
import Data.List (stripPrefix)
import Data.Char (isUpper, toLower)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options, defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Endpoints
--------------------------------------------------------------------------------

type TodoApi = "entries" :>
  (    CreateEntry
  :<|> ReadEntries
  :<|> UpdateEntry
  :<|> DeleteEntry
  )

type CreateEntry =
     ReqBody '[JSON] EntryInfo
  :> Post '[JSON] AddEntryResult

type ReadEntries =
     Get '[JSON] (Map Int EntryInfo)

type UpdateEntry =
     CaptureEntryId
  :> ReqBody '[JSON] EntryInfo
  :> Put '[JSON] NoContent

type DeleteEntry =
     CaptureEntryId
  :> Delete '[JSON] NoContent

type CaptureEntryId = Capture "eid" Int


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data EntryInfo =
     EntryInfo
     { _entryInfDescription :: !T.Text
     , _entryInfCompleted   :: !Bool
     } deriving (Show, Generic)

instance ToJSON EntryInfo where
    toJSON = genericToJSON $ optionsDelPrefix "_entryInf"

instance FromJSON EntryInfo where
    parseJSON = genericParseJSON $ optionsDelPrefix "_entryInf"

data AddEntryResult = AddEntryResult { _addEntryResultId :: !Int }
     deriving (Show, Generic)

instance ToJSON AddEntryResult where
    toJSON = genericToJSON $ optionsDelPrefix "_addEntryResult"

instance FromJSON AddEntryResult where
    parseJSON = genericParseJSON $ optionsDelPrefix "_addEntryResult"


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

optionsDelPrefix :: String -> Options
optionsDelPrefix prefix =
    defaultOptions{fieldLabelModifier = delPrefix prefix}

delPrefix :: String -> (String -> String)
delPrefix "" = id
delPrefix prefix = \fieldName ->
    case stripPrefix prefix fieldName of
      Just (c:cs)
          | isUpper c -> toLower c : cs
          | otherwise -> error $ "The field name after the prefix "
                              ++ "must be written in CamelCase"
      Just "" -> error $ "The field name after the prefix may not be empty"
      Nothing -> error $  "The field name " ++ quotes fieldName
                      ++ " does not begin with the required prefix "
                      ++ quotes prefix

quotes :: String -> String
quotes s = "\"" ++ s ++ "\""


makeLenses ''EntryInfo
makeLenses ''AddEntryResult
