{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NumDecimals                #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Writer ( tell )
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict hiding (tell)
import           Control.Lens hiding (view)
import qualified Control.Lens as L
import           Data.Aeson   hiding (Object, (.=))
import           Data.Bool
import qualified Data.Map     as M
import           Data.Monoid
import           GHC.Generics
import           Miso
import           Miso.String  (MisoString, toMisoString)
import qualified Miso.String  as S
import           Nixtodo.Api
import           Nixtodo.Api.Client
import qualified Data.Text as T
import           Data.JSString.Text (textFromJSString)
import qualified Data.JSString as JSS
import           Servant.Client.Ghcjs
import qualified JavaScript.Web.Location
import           GHCJS.DOM ( currentWindowUnchecked )
import           GHCJS.DOM.Window ( getLocation )
import           GHCJS.DOM.Location ( getHostname, getPort )


data Model = Model
  { _entries    :: ![EntryRecord]
  , _field      :: !T.Text
  , _uid        :: !Int
  , _visibility :: !Visibility
  } deriving (Show, Generic, Eq)

data EntryRecord = EntryRecord
  { _entryRecEntry    :: !Entry
  , _entryRecEditing  :: !Bool
  , _entryRecFocussed :: !Bool
  } deriving (Show, Generic, Eq)

data Visibility = ViewAll | ViewActive | ViewCompleted
                  deriving (Show, Generic, Eq)

makeLenses ''Model
makeLenses ''EntryRecord

instance ToJSON EntryRecord
instance ToJSON Model
instance ToJSON Visibility

instance FromJSON EntryRecord
instance FromJSON Model
instance FromJSON Visibility

eid :: Lens' EntryRecord EntryId
eid = entryRecEntry . entryId

description :: Lens' EntryRecord T.Text
description = entryRecEntry . entryEntry . entryInfDescription

completed :: Lens' EntryRecord Bool
completed = entryRecEntry . entryEntry . entryInfCompleted

emptyModel :: Model
emptyModel = Model
  { _entries    = []
  , _visibility = ViewAll
  , _field      = mempty
  , _uid        = 0
  }

newEntry :: T.Text -> Int -> EntryRecord
newEntry desc id' = EntryRecord
  { _entryRecEntry = Entry
      { _entryId = id'
      , _entryEntry = EntryInfo
          { _entryInfDescription = desc
          , _entryInfCompleted   = False
          }
      }
  , _entryRecEditing  = False
  , _entryRecFocussed = False
  }

data Action
  = NoOp
  | Initialize
  | SetEntries ![Entry]
  | WebSocketEvent !(Miso.WebSocket EntryEvent) -- TODO
  | UpdateField !T.Text
  | EditingEntry !Int !Bool
  | UpdateEntry !Int !T.Text
  | Add
  | Delete !Int
  | DeleteComplete
  | Check !Int !Bool
  | CheckAll !Bool
  | ChangeVisibility !Visibility

main :: IO ()
main = do
    websocketUrl <- getWebsocketUrl
    startApp App
      { initialAction = Initialize
      , model  = emptyModel
      , view   = viewModel
      , update = fromTransition . updateModel
      , events = defaultEvents
      , subs = [websocketSub (URL websocketUrl) (Protocols []) WebSocketEvent]
      }
  where
    getWebsocketUrl :: IO MisoString
    getWebsocketUrl = do
      window <- currentWindowUnchecked
      location <- getLocation window
      host <- getHostname location
      port <- getPort location
      pure $ "wss://" <> host <> ":" <> port <> "/websocket"

updateModel :: Action -> Transition Action Model ()
updateModel = \case
    NoOp -> pure ()

    Initialize -> scheduleIO $ do
        result <- callServant "" $ readEntries nixtodoApiClient
        case result of
          Left err -> do
            print err
            threadDelay 1e6
            pure Initialize
          Right entries -> pure $ SetEntries entries

    SetEntries _entries -> pure ()

    WebSocketEvent webSocketAction ->
        case webSocketAction of
          WebSocketMessage entryEvent ->
              case entryEvent of
                UpsertEntryEvent entry -> pure () -- TODO !!!
                DeleteEntryEvent entryId -> pure () -- TODO !!!
          WebSocketClose _closeCode _wasClean _reason -> pure ()
          WebSocketOpen                               -> pure ()
          WebSocketError err                          -> pure ()

    Add -> do
      oldUid   <- use uid
      oldField <- use field

      uid   .= oldUid + 1
      field .= mempty

      unless (T.null oldField) $ do
        entries %= (<> [newEntry oldField oldUid])

        scheduleIO $ do
          _result <- callServant "" $ createEntry nixtodoApiClient
            EntryInfo
            { _entryInfDescription = oldField
            , _entryInfCompleted   = False
            }
          pure NoOp

    UpdateField str -> field .= str

    EditingEntry id' isEditing -> do
      entries %= filterMap ((== id') . L.view eid)
                   ( (entryRecEditing  .~ isEditing)
                   . (entryRecFocussed .~ isEditing)
                   )
      scheduleIO $ NoOp <$ focus ("todo-" <> S.pack (show id'))

    UpdateEntry id' desc -> do
      entries %= filterMap ((== id') . L.view eid)
                   (description .~ desc)

      scheduleIO $ do
        _result <- callServant "" $ updateEntry nixtodoApiClient id'
          EntryInfo
          { _entryInfDescription = desc
          , _entryInfCompleted   = False -- TODO !!!
          }
        pure NoOp

    Delete id' -> do
      entries %= filter ((/= id') . L.view eid)

      scheduleIO $ do
        _result <- callServant "" $ deleteEntry nixtodoApiClient id'
        pure NoOp

    DeleteComplete ->
      entries %= filter (not . L.view completed)

    Check id' isCompleted -> do
      entries %= filterMap ((== id') . L.view eid)
                   (completed .~ isCompleted)
      scheduleIO $ do
        _result <- callServant "" $ updateEntry nixtodoApiClient id'
          EntryInfo
          { _entryInfDescription = "" -- TODO !!!
          , _entryInfCompleted   = isCompleted
          }
        pure NoOp

    CheckAll isCompleted ->
      entries %= filterMap (const True)
                   (completed .~ isCompleted)

    ChangeVisibility v ->
      visibility .= v

filterMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterMap predicate f = go'
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   =   y : go' ys

viewModel :: Model -> View Action
viewModel m =
 div_
    [ class_ "todomvc-wrapper"
    , style_  $ M.singleton "visibility" "hidden"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m (m ^. field)
        , viewEntries (m ^. visibility) (m ^. entries)
        , viewControls m (m ^. visibility) (m ^. entries)
        ]
    , infoFooter
    ]

viewEntries :: Visibility -> [ EntryRecord ] -> View Action
viewEntries visibility entries =
  section_
    [ class_ "main"
    , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , name_ "toggle"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ] []
      , label_
        [ for_ "toggle-all" ]
          [ text $ S.pack "Mark all as complete" ]
      , ul_ [ class_ "todo-list" ] $
         flip map (filter isVisible entries) $ \t ->
           viewKeyedEntry t
      ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all (L.view completed) entries
    isVisible entryRec =
      case visibility of
        ViewCompleted ->       entryRec ^. completed
        ViewActive    -> not $ entryRec ^. completed
        _ -> True

viewKeyedEntry :: EntryRecord -> View Action
viewKeyedEntry = viewEntry

viewEntry :: EntryRecord -> View Action
viewEntry entryRec = liKeyed_ (toKey $ entryRec ^. eid)
    [ class_ $ S.intercalate " " $
       [ "completed" | entryRec ^. completed ] <> [ "editing" | entryRec ^. entryRecEditing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ $ entryRec ^. completed
            , onClick $ Check (entryRec ^. eid) (not $ entryRec ^. completed)
            ] []
        , label_
            [ onDoubleClick $ EditingEntry (entryRec ^. eid) True ]
            [ text $ toMisoString $ entryRec ^. description ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete (entryRec ^. eid)
            ] []
        ]
    , input_
        [ class_ "edit"
        , value_ (toMisoString $ entryRec ^. description)
        , name_ "title"
        , id_ $ "todo-" <> S.pack (show $ entryRec ^. eid)
        , onInput $ UpdateEntry (entryRec ^. eid) . textFromJSString
        , onBlur $ EditingEntry (entryRec ^. eid) False
        , onEnter $ EditingEntry (entryRec ^. eid) False
        ]
        []
    ]

viewControls :: Model ->  Visibility -> [ EntryRecord ] -> View Action
viewControls model visibility entries =
  footer_ [ class_ "footer"
          , hidden_ (bool "" "hidden" $ null entries)
          ]
      [ viewControlsCount entriesLeft
      , viewControlsFilters visibility
      , viewControlsClear model entriesCompleted
      ]
  where
    entriesCompleted = length . filter (L.view completed) $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> View Action
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text $ S.pack (show entriesLeft) ]
     , text (item_ <> " left")
     ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: Visibility -> View Action
viewControlsFilters visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap "#/" ViewAll visibility
    , text " "
    , visibilitySwap "#/active" ViewActive visibility
    , text " "
    , visibilitySwap "#/completed" ViewCompleted visibility
    ]

visibilitySwap :: MisoString -> Visibility -> Visibility -> View Action
visibilitySwap uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ uri
           , class_ $ S.concat [ "selected" | visibility == actualVisibility ]
           , onClick (ChangeVisibility visibility)
           ] [ viewVisibility visibility ]
      ]

viewVisibility :: Visibility -> View a
viewVisibility = text . \case
  ViewAll       -> "All"
  ViewActive    -> "Active"
  ViewCompleted -> "Completed"

viewControlsClear :: Model -> Int -> View Action
viewControlsClear _ entriesCompleted =
  button_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text $ "Clear completed (" <> S.pack (show entriesCompleted) <> ")" ]

viewInput :: Model -> T.Text -> View Action
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ $ toMisoString task
        , name_ "newTodo"
        , onInput $ UpdateField . textFromJSString
        , onEnter Add
        ] []
    ]

onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

infoFooter :: View a
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text "Double-click to edit a todo" ]
    , p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/dmjio" ] [ text "David Johnson" ]
        ]
    , p_ []
        [ text "Adapted by "
        , a_ [ href_ "https://github.com/basvandijk" ] [ text "Bas van Dijk" ]
        ]
    , p_ []
        [ text "for the "
        , a_ [ href_ "https://github.com/basvandijk/nix-workshop" ]
             [ text "Nix Workshop @ Haskell eXchange 2017" ]
        ]
    , p_ []
        [ text "Part of "
        , a_ [ href_ "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]

--------------------------------------------------------------------------------

-- | Performs blocking AJAX request on the location of the browser window
callServant
    :: String
       -- ^ Path prefixed to HTTP requests.
    -> ClientM a
    -> IO (Either ServantError a)
callServant path m = do
    curLoc <- JavaScript.Web.Location.getWindowLocation

    jsStr_protocol <- JavaScript.Web.Location.getProtocol curLoc
    jsStr_port     <- JavaScript.Web.Location.getPort     curLoc
    jsStr_hostname <- JavaScript.Web.Location.getHostname curLoc

    let protocol
          | jsStr_protocol == "https:" = Https
          | otherwise                  = Http

        portStr :: String
        portStr = JSS.unpack jsStr_port

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

        hostname :: String
        hostname = JSS.unpack jsStr_hostname

    runClientM m (ClientEnv (BaseUrl protocol hostname port path))
