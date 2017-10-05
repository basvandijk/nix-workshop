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

module Main where

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
import           Miso.String  (MisoString)
import qualified Miso.String  as S

default (MisoString)

data Model = Model
  { _entries    :: ![Entry]
  , _field      :: !MisoString
  , _uid        :: !Int
  , _visibility :: !Visibility
  } deriving (Show, Generic, Eq)

data Entry = Entry
  { _description :: !MisoString
  , _completed   :: !Bool
  , _editing     :: !Bool
  , _eid         :: !Int
  , _focussed    :: !Bool
  } deriving (Show, Generic, Eq)

data Visibility = ViewAll | ViewActive | ViewCompleted
                  deriving (Show, Generic, Eq)

makeLenses ''Model
makeLenses ''Entry

instance ToJSON Entry
instance ToJSON Model
instance ToJSON Visibility

instance FromJSON Entry
instance FromJSON Model
instance FromJSON Visibility

emptyModel :: Model
emptyModel = Model
  { _entries    = []
  , _visibility = ViewAll
  , _field      = mempty
  , _uid        = 0
  }

newEntry :: MisoString -> Int -> Entry
newEntry desc eid = Entry
  { _description = desc
  , _completed   = False
  , _editing     = False
  , _eid         = eid
  , _focussed    = False
  }

data Action
  = NoOp
  | UpdateField !MisoString
  | EditingEntry !Int !Bool
  | UpdateEntry !Int !MisoString
  | Add
  | Delete !Int
  | DeleteComplete
  | Check !Int !Bool
  | CheckAll !Bool
  | ChangeVisibility !Visibility
   deriving Show

main :: IO ()
main = startApp App{ initialAction = NoOp
                   , model         = emptyModel
                   , view          = viewModel
                   , update        = fromTransition . updateModel
                   , events        = defaultEvents
                   , subs          = []
                   }

updateModel :: Action -> Transition Action Model ()
updateModel = \case
    NoOp -> pure ()

    Add -> do
      oldUid   <- use uid
      oldField <- use field

      uid   .= oldUid + 1
      field .= mempty

      unless (S.null oldField) $
        entries %= (<> [newEntry oldField oldUid])

    UpdateField str -> field .= str

    EditingEntry id' isEditing -> do
      entries %= filterMap ((== id') . L.view eid)
                   ( (editing  .~ isEditing)
                   . (focussed .~ isEditing)
                   )
      scheduleIO $ NoOp <$ focus ("todo-" <> S.pack (show id'))

    UpdateEntry id' task ->
      entries %= filterMap ((== id') . L.view eid)
                   (description .~ task)

    Delete id' ->
      entries %= filter ((/= id') . L.view eid)

    DeleteComplete ->
      entries %= filter (not . L.view completed)

    Check id' isCompleted -> do
      entries %= filterMap ((== id') . L.view eid)
                   (completed .~ isCompleted)
      scheduleIO $ NoOp <$ putStrLn "clicked check"

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

viewEntries :: Visibility -> [ Entry ] -> View Action
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
    isVisible entry =
      case visibility of
        ViewCompleted ->       entry ^. completed
        ViewActive    -> not $ entry ^. completed
        _ -> True

viewKeyedEntry :: Entry -> View Action
viewKeyedEntry = viewEntry

viewEntry :: Entry -> View Action
viewEntry entry = liKeyed_ (toKey $ entry ^. eid)
    [ class_ $ S.intercalate " " $
       [ "completed" | entry ^. completed ] <> [ "editing" | entry ^. editing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ $ entry ^. completed
            , onClick $ Check (entry ^. eid) (not $ entry ^. completed)
            ] []
        , label_
            [ onDoubleClick $ EditingEntry (entry ^. eid) True ]
            [ text $ entry ^. description ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete (entry ^. eid)
            ] []
        ]
    , input_
        [ class_ "edit"
        , value_ (entry ^. description)
        , name_ "title"
        , id_ $ "todo-" <> S.pack (show $ entry ^. eid)
        , onInput $ UpdateEntry (entry ^. eid)
        , onBlur $ EditingEntry (entry ^. eid) False
        , onEnter $ EditingEntry (entry ^. eid) False
        ]
        []
    ]

viewControls :: Model ->  Visibility -> [ Entry ] -> View Action
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

viewInput :: Model -> MisoString -> View Action
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ task
        , name_ "newTodo"
        , onInput UpdateField
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
