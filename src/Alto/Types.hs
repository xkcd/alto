{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Alto.Types where

import           Control.Lens.TH
import           Control.Monad.State (StateT)
import qualified Data.Aeson as JS
import qualified Data.Aeson.TH as JS
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import           Data.Char (toLower)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

type MenuID = Text

newtype Tag =
  Tag Text
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ClientState =
  ClientState
  { _clientTags :: Set Tag
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''ClientState
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 7, JS.constructorTagModifier = map toLower} ''ClientState

{-
data Event =
   ReloadRoot
 | ChangeMenu
 | ChangeClientState
-}

data GlobalMenuState =
  GMS
  { _clientState :: ClientState
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''GlobalMenuState

data EntryDisplay =
   Always
 | WhenSet Tag
 | WhenNotSet Tag
 | InactiveWhen EntryDisplay
 deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

data EntryType =
   Action { _setTags :: Set Tag, _unsetTags :: Set Tag }
   -- ^ When the entry is clicked it does the above
 | SubMenu { _subMenu :: MenuID, _setTags :: Set Tag, _unsetTags :: Set Tag }
   -- ^ When the entry is selected, the submenu is displayed
 -- | CallBack SomeHMACedThing
 deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''EntryType
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1, JS.sumEncoding = JS.UntaggedValue} ''EntryType

data MenuEntry =
  MEntry
  { _icon :: Maybe Text
  , _label :: Text
  , _reaction :: EntryType
  , _display :: EntryDisplay
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''MenuEntry
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1} ''MenuEntry

instance IsString MenuEntry where
  fromString l = MEntry Nothing (T.pack l) (Action mempty mempty) Always 

data Menu =
  Menu
  { _mid :: MenuID
  , _entries :: [MenuEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''Menu
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = dropWhile (=='m') . drop 1} ''Menu

data Root =
  MenuRoot
  { _rootState :: ClientState
  , _rootMenu :: Menu
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''Root
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 5} ''Root

data MenuSystem =
  MenuSystem
  { _menuMap :: Map MenuID Menu
  , _globalState :: GlobalMenuState
  , _topMenu :: Menu
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''MenuSystem

data CompState =
  CSt
  { _salt :: ByteString
    -- ^ A pseudo-salt derived expensively from the overall name. 
  , _menus :: Map MenuID Menu
  , _tags :: Set Tag
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''CompState
