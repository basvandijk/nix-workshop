{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}

module Todo.Api.Client where

import Data.Proxy
import Todo.Api
import Servant.Client.Core
import Servant.API

data TodoApiClient m
   = TodoApiClient
     { createEntry :: !(Client m CreateEntry)
     , readEntries :: !(Client m ReadEntries)
     , updateEntry :: !(Client m UpdateEntry)
     , deleteEntry :: !(Client m DeleteEntry)
     }

todoApiClient :: forall m . HasClient m TodoApi => TodoApiClient m
todoApiClient = TodoApiClient{..}
  where
    (      createEntry
      :<|> readEntries
      :<|> updateEntry
      :<|> deleteEntry
     ):<|> _websocketApi
      :<|> _frontendApi = Proxy @TodoApi `clientIn` Proxy @m
