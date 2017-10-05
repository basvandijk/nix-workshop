{-# language OverloadedStrings #-}

module Main (main) where

import Control.Monad.Managed.Safe (runManaged, managed_)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as O
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid ((<>))
import Control.Applicative (many)
import qualified Data.Text as T
import qualified Todo.Backend.Database as Db
import qualified Todo.Backend.WebServer as WebServer

main :: IO ()
main = do
    cfg <- do
      configFiles <- O.execParser opts
      C.load $ map C.Required configFiles

    let subCfg :: T.Text -> C.Config
        subCfg sectionName = C.subconfig sectionName cfg

    db <- do
      c <- Db.parseConfig (subCfg "database")
      Db.new c

    do
      c <- WebServer.parseConfig (subCfg "web-server")
      WebServer.serve c
  where
    opts :: O.ParserInfo [FilePath]
    opts = O.info (O.helper <*> options)
      (  O.fullDesc
      <> O.progDesc "The TODO-list backend server"
      )

    options :: O.Parser [FilePath]
    options = many (O.strOption (  O.long "config"
                                <> O.short 'c'
                                <> O.help "Configuration files to load"
                                )
                   )
