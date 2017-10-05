{-# language OverloadedStrings #-}
{-# language TypeApplications  #-}

module Todo.Backend.WebServer
    ( -- * Configuration
      Config(..)
    , parseConfig

    -- * Serving
    , serve
    ) where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, GzipFiles(..) )
import qualified Todo.Backend.Database as Db
import Todo.Api
import qualified Servant
import Data.Proxy (Proxy(Proxy))
import Data.Default (def)

data Config
   = Config
     { cfgWarpSettings :: !Warp.Settings
     }

parseConfig
    :: C.Config
    -> IO Config
parseConfig cfg  = do
    warpSettings <- mkWarpSettings cfg
    pure Config
         { cfgWarpSettings   = warpSettings
         }

mkWarpSettings :: C.Config -> IO Warp.Settings
mkWarpSettings cfg = do
    port <- read <$> C.require cfg "port"
    host <- C.require cfg "host"
    pure $ Warp.setPort port
         $ Warp.setHost (fromString host)
         $ Warp.defaultSettings

serve
    :: Config
    -> Db.Handle
    -> IO ()
serve cfg db =
    Warp.runSettings (cfgWarpSettings cfg) $
      gzip gzipSettings $ Servant.serve (Proxy @TodoApi) todoServer
  where
    gzipSettings = def{gzipFiles = GzipPreCompressed GzipCompress}

    todoServer :: Servant.Server TodoApi
    todoServer = _todo
