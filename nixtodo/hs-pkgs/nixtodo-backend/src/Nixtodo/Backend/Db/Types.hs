{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Nixtodo.Backend.Db.Types where

import Opaleye
import Data.Profunctor.Product.TH ( makeAdaptorAndInstance )
import Control.Lens ( makeLenses )
import qualified Data.Text as T
import Nixtodo.Api

type DBEntryInfo = EntryInfo'
                     (Column PGText)
                     (Column PGBool)

entriesTable :: Table (Entry' (Maybe (Column PGInt4)) DBEntryInfo)
                      (Entry' (Column PGInt4) DBEntryInfo)
entriesTable =
    Table "entries" $
      pEntry Entry
        { _entryId    = optional "id"
        , _entryEntry =
            pEntryInfo EntryInfo
              { _entryInfDescription = required "description"
              , _entryInfCompleted   = required "completed"
              }
        }
