{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}

module Nixtodo.Api.Client where

import Data.Proxy
import Nixtodo.Api
import Servant.Client.Core
import Servant.API

data NixtodoApiClient m
   = NixtodoApiClient
     { createEntry :: !(Client m CreateEntry)
     , readEntries :: !(Client m ReadEntries)
     , updateEntry :: !(Client m UpdateEntry)
     , deleteEntry :: !(Client m DeleteEntry)
     }

nixtodoApiClient :: forall m . HasClient m NixtodoApi => NixtodoApiClient m
nixtodoApiClient = NixtodoApiClient{..}
  where
    (      createEntry
      :<|> readEntries
      :<|> updateEntry
      :<|> deleteEntry
     ):<|> _websocketApi
      :<|> _frontendApi = Proxy @NixtodoApi `clientIn` Proxy @m
