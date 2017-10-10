{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Arrows #-}

module Nixtodo.Backend.Db
  ( -- * Configuration
    Config(..)
  , PoolConfig(..)
  , parseConfig

    -- * Initialization
  , Handle
  , with

    -- * API
  , createEntry
  , readEntries
  , updateEntry
  , deleteEntry

    -- * Events
  , EventListener
  , getEventListener
  , getEvent
  ) where

import "async" Control.Concurrent.Async (withAsyncWithUnmask)
import "base" Control.Monad.IO.Class (liftIO)
import "base" Control.Arrow (returnA)
import "base" Control.Monad (forever)
import "base" Data.Foldable (for_)
import Control.Lens
import Control.Monad (void)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Database.PostgreSQL.Simple as Pg
import           Data.Pool (Pool, LocalPool, withResource)
import qualified Data.Pool as Pool
import qualified Data.Text    as T
import qualified Data.Text.IO as T (readFile)
import Nixtodo.Api
import Data.Map (Map)
import qualified Data.Map.Strict as M (fromList)
import Nixtodo.Backend.Db.Types
import Opaleye
import "stm" Control.Concurrent.STM.TChan
import "stm" Control.Monad.STM (atomically)
import "managed" Control.Monad.Managed.Safe ( Managed, managed )
import qualified "bytestring" Data.ByteString.Char8 as BC8
import "postgresql-simple" Database.PostgreSQL.Simple.Notification


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data Config
   = Config
     { cfgPoolConfig  :: !PoolConfig
     , cfgConnectInfo :: !Pg.ConnectInfo
     }

data PoolConfig
   = PoolConfig
     { poolCfgNumStripes   :: !Int
     , poolCfgMaxResources :: !Int
     , poolCfgIdleTime     :: !Integer
     }

parseConfig :: C.Config -> IO Config
parseConfig cfg =
    Config <$> parsePoolConfig (C.subconfig "pool" cfg)
           <*> parseConnectInfoConfig cfg
  where
    parsePoolConfig :: C.Config -> IO PoolConfig
    parsePoolConfig cfg =
        PoolConfig <$> C.require cfg "numStripes"
                   <*> C.require cfg "maxResources"
                   <*> C.require cfg "idleTime"

    parseConnectInfoConfig :: C.Config -> IO Pg.ConnectInfo
    parseConnectInfoConfig cfg =  do
        host         <- C.require cfg "host"
        port         <- read <$> C.require cfg "port"
        user         <- C.require cfg "user"
        passwordFile <- C.require cfg "passwordFile"
        database     <- C.require cfg "database"

        password <- T.readFile passwordFile

        return Pg.ConnectInfo
               { Pg.connectHost     = host
               , Pg.connectPort     = port
               , Pg.connectUser     = user
               , Pg.connectPassword = T.unpack password
               , Pg.connectDatabase = database
               }


--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

data Handle =
     Handle
     { hndlPgConnPool :: !(Pool Pg.Connection)
     , hndlEventChan  :: !(TChan EntryEvent)
     }

with :: Config -> Managed Handle
with config = do
    pgConnPool <- liftIO $ Pool.createPool
        (Pg.connect $ cfgConnectInfo config)
        Pg.close
        (poolCfgNumStripes poolConfig)
        (fromIntegral $ poolCfgIdleTime poolConfig)
        (poolCfgMaxResources poolConfig)

    eventChan <- liftIO $ newBroadcastTChanIO

    let hndl = Handle
               { hndlPgConnPool = pgConnPool
               , hndlEventChan  = eventChan
               }

    conn <- managed $ withResource pgConnPool
    _async <- managed $ withAsyncWithUnmask $ \unmask -> unmask $ do
                void $ Pg.execute_ conn $ "LISTEN event_channel"
                forwardEvents conn hndl
    pure hndl
  where
    poolConfig = cfgPoolConfig config

forwardEvents :: Pg.Connection -> Handle -> IO ()
forwardEvents conn hndl = forever $ do
    not <- getNotification conn
    print $ notificationData not
    for_ (parseNotificationData $ notificationData not) $ \(operation, eid) ->
      case operation of
        DELETE -> atomically $ writeTChan eventChan $ DeleteEntryEvent eid
        UPSERT -> do
          mbEntry <- lookupEntry hndl eid
          for_ mbEntry $ \entry ->
            atomically $ writeTChan eventChan $ UpsertEntryEvent entry
  where
    eventChan = hndlEventChan hndl

data Operation = UPSERT | DELETE

parseNotificationData :: BC8.ByteString -> Maybe (Operation, EntryId)
parseNotificationData bs = do
    let (operationBs, restBs) = BC8.break (== ':') bs
    operation <- case operationBs of
                   "INSERT" -> Just UPSERT
                   "UPDATE" -> Just UPSERT
                   "DELETE" -> Just DELETE
                   _        -> Nothing
    (':', eidBs) <- BC8.uncons restBs
    (eid, "") <- BC8.readInt eidBs
    pure (operation, eid)


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

createEntry :: Handle -> EntryInfo -> IO Entry
createEntry hndl entryInf = do
    entries <- withResource (hndlPgConnPool hndl) $ \conn ->
      runInsertReturning conn entriesTable
        Entry{ _entryId    = Nothing
             , _entryEntry =
                 EntryInfo
                 { _entryInfDescription = constant $ entryInf ^. entryInfDescription
                 , _entryInfCompleted   = constant $ entryInf ^. entryInfCompleted
                 }
             }
        id
    case entries of
      [entry] -> pure entry
      _ -> error "todo"

readEntries :: Handle -> IO [Entry]
readEntries hndl =
    withResource (hndlPgConnPool hndl) $ \conn -> runQuery conn $
      queryTable entriesTable

lookupEntry :: Handle -> EntryId -> IO (Maybe Entry)
lookupEntry hndl eid = do
    entries <- withResource (hndlPgConnPool hndl) $ \conn -> runQuery conn $ proc () -> do
      entry <- queryTable entriesTable -< ()
      restrict -< entry ^. entryId .=== constant eid
      returnA -< entry
    case entries of
      [entry] -> pure $ Just entry
      []      -> pure Nothing
      _       -> error "Multiple rows returned!"

updateEntry :: Handle -> EntryId -> EntryInfo -> IO ()
updateEntry hndl eid entryInf =
    void $ withResource (hndlPgConnPool hndl) $ \conn ->
      runUpdate conn entriesTable
      (\_entry -> Entry
        { _entryId    = Nothing -- Just $ constant eid
        , _entryEntry = constant entryInf
        })
      (\entry -> entry ^. entryId .=== constant eid)

deleteEntry :: Handle -> EntryId -> IO ()
deleteEntry hndl eid =
    void $ withResource (hndlPgConnPool hndl) $ \conn ->
      runDelete conn entriesTable $ \entry ->
        entry ^. entryId .=== constant eid


--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

newtype EventListener = EventListener (TChan EntryEvent)

getEventListener :: Handle -> IO EventListener
getEventListener hndl = fmap EventListener $ atomically $
                          dupTChan $ hndlEventChan hndl

getEvent :: EventListener -> IO EntryEvent
getEvent (EventListener eventChan) = atomically $ readTChan eventChan
