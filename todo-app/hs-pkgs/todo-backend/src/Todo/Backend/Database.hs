{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}

module Todo.Backend.Database
  ( -- * Configuration
    Config(..)
  , PoolConfig(..)
  , Pg.ConnectInfo(..)

  , parseConfig
  , parsePoolConfig
  , parseConnectInfoConfig

    -- * Handle
  , Handle
  , new

  ) where

import Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Database.PostgreSQL.Simple as Pg
import           Data.Pool (Pool, LocalPool)
import qualified Data.Pool as Pool
import qualified Data.Text    as T
import qualified Data.Text.IO as T (readFile)

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

newtype Handle
   = Handle
     { _hndlPgConnPool :: (Pool Pg.Connection)
     }

makeLenses ''Handle

new :: Config
    -> IO Handle
new config = do
    pgConnPool <- Pool.createPool
        (Pg.connect $ cfgConnectInfo config)
        Pg.close
        (poolCfgNumStripes poolConfig)
        (fromIntegral $ poolCfgIdleTime poolConfig)
        (poolCfgMaxResources poolConfig)

    pure Handle{_hndlPgConnPool = pgConnPool}
  where
    poolConfig = cfgPoolConfig config
