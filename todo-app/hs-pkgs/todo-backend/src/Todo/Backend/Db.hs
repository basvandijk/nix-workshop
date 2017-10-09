{-# language OverloadedStrings #-}

module Todo.Backend.Db
  ( -- * Configuration
    Config(..)
  , PoolConfig(..)
  , parseConfig

    -- * Initialization
  , Handle
  , new

    -- * API
  , createEntry
  , readEntries
  , updateEntry
  , deleteEntry
  ) where

import Control.Lens
import Control.Monad (void)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Database.PostgreSQL.Simple as Pg
import           Data.Pool (Pool, LocalPool, withResource)
import qualified Data.Pool as Pool
import qualified Data.Text    as T
import qualified Data.Text.IO as T (readFile)
import Todo.Api
import Data.Map (Map)
import qualified Data.Map.Strict as M (fromList)
import Todo.Backend.Db.Types
import Opaleye

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

newtype Handle = Handle { hndlPgConnPool :: Pool Pg.Connection }

new :: Config -> IO Handle
new config = do
    pgConnPool <- Pool.createPool
        (Pg.connect $ cfgConnectInfo config)
        Pg.close
        (poolCfgNumStripes poolConfig)
        (fromIntegral $ poolCfgIdleTime poolConfig)
        (poolCfgMaxResources poolConfig)

    pure Handle{hndlPgConnPool = pgConnPool}
  where
    poolConfig = cfgPoolConfig config


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
