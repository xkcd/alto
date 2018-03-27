{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Alto.Types where

import           Control.Lens.TH
import           Control.Monad.State (StateT)
import qualified Data.Aeson as JSON
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

type MenuID = Text

newtype Tag = Tag Text

data ClientState =
  ClientState
  { _clientTags :: Set Tag
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''ClientState

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
 deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Reaction =
   Inactive
 | SubMenu MenuID
 | ChangeTags { _setTags :: Set Tag, _unsetTags :: Set Tag }
 -- | CallBack SomeHMACedThing
 deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Reaction

data MenuEntry =
  MEntry
  { _icon :: Maybe Text
  , _label :: Text
  , _reaction :: Reaction
  , _display :: EntryDisplay
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''MenuEntry

instance IsString MenuEntry where
  fromString l = MEntry Nothing (T.pack l) Inactive Always

data Menu =
  Menu
  { _mid :: MenuID
  , _entries :: [MenuEntry]
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Menu

data Root =
  MenuRoot
  { _rootState :: ClientState
  , _rootMenu :: Menu
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Root

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

type MenuM a = StateT CompState IO a
