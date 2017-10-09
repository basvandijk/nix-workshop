{-# language OverloadedStrings #-}
{-# language TypeApplications  #-}
{-# language PackageImports  #-}

module Todo.Backend.WebServer
    ( -- * Configuration
      Config(..)
    , parseConfig

    -- * Serving
    , serve
    ) where

import System.FilePath ( addTrailingPathSeparator )
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, GzipFiles(..) )
import qualified Todo.Backend.Db as Db
import qualified Todo.Backend.IndexTemplater as IndexTemplater
import Todo.Api
import qualified Servant
import Servant.API
import Data.Proxy (Proxy(Proxy))
import Data.Default (def)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Application.Static
    ( StaticSettings, ssMaxAge, defaultFileServerSettings, staticApp )
import WaiAppStatic.Types ( MaxAge(MaxAgeForever) )
import Data.Tagged (Tagged(..))
import qualified "wai-websockets" Network.Wai.Handler.WebSockets as WebSockets
import qualified "websockets" Network.WebSockets.Connection as WebSockets
import qualified "wai" Network.Wai as Wai
import qualified "http-types" Network.HTTP.Types.Status as Http


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data Config = Config { cfgWarpSettings :: !Warp.Settings }

parseConfig
    :: C.Config
    -> IO Config
parseConfig cfg  = do
    warpSettings <- mkWarpSettings cfg
    pure Config{ cfgWarpSettings = warpSettings }
  where
    mkWarpSettings :: C.Config -> IO Warp.Settings
    mkWarpSettings cfg = do
        port <- read <$> C.require cfg "port"
        host <- C.require cfg "host"
        pure $ Warp.setPort port
             $ Warp.setHost (fromString host)
             $ Warp.defaultSettings


--------------------------------------------------------------------------------
-- Serving
--------------------------------------------------------------------------------

serve :: Config -> Db.Handle -> IndexTemplater.Handle -> IO ()
serve cfg db frontendIndexTemplater =
    Warp.runSettings (cfgWarpSettings cfg) $
      gzip gzipSettings $
        Servant.serve (Proxy @TodoApi) todoServer
  where
    gzipSettings = def{gzipFiles = GzipPreCompressed GzipCompress}

    todoServer :: Servant.Server TodoApi
    todoServer =
        (      createEntryServer
          :<|> readEntriesServer
          :<|> updateEntryServer
          :<|> deleteEntryServer
        ) :<|> websocketServer
          :<|> frontendServer

    createEntryServer :: Servant.Server CreateEntry
    createEntryServer entryInfo = do
      liftIO $ Db.createEntry db entryInfo

    readEntriesServer :: Servant.Server ReadEntries
    readEntriesServer = do
      liftIO $ Db.readEntries db

    updateEntryServer :: Servant.Server UpdateEntry
    updateEntryServer entryId entryInfo = do
      liftIO $ Db.updateEntry db entryId entryInfo
      pure NoContent

    deleteEntryServer :: Servant.Server DeleteEntry
    deleteEntryServer entryId = do
      liftIO $ Db.deleteEntry db entryId
      pure NoContent

    websocketServer :: Servant.Server Raw
    websocketServer =
        Tagged $ WebSockets.websocketsOr opts listener nonSocket
      where
        nonSocket _req respond =
            respond $ Wai.responseLBS
                Http.notAcceptable406
                [("Content-Type", "text/plain")]
                "This is a websocket route. Connect to it using websockets."

        opts = WebSockets.ConnectionOptions $ pure ()

        listener :: WebSockets.PendingConnection -> IO ()
        listener pconn = do
            conn <- WebSockets.acceptRequest pconn

            -- WebSockets.sendTextData conn $ Json.encode eventGroup
            pure ()

    frontendServer :: Servant.Server FrontendApi
    frontendServer =
             serveStatic              frontendIndexTemplater
        :<|> IndexTemplater.getHashed frontendIndexTemplater
        :<|> IndexTemplater.getClient frontendIndexTemplater
      where
        serveStatic :: IndexTemplater.Handle -> Servant.Server GetStatic
        serveStatic indexTemplater =
          Tagged $ staticApp $
            (defaultFileServerSettings
               (addTrailingPathSeparator
                 (IndexTemplater.cfgSrcDir
                   (IndexTemplater.getConfig indexTemplater))))
            { ssMaxAge = MaxAgeForever }
