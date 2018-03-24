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
import           Data.Text (Text)
import           GHC.Generics

type MenuID = Text

type ClientState = ()

{-
data Event =
   ReloadRoot
 | ChangeMenu
 | ChangeClientState
-}

data GlobalMenuState =
  GMS
  { _clientState :: ClientState
  , _privateState :: ()
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''GlobalMenuState

data Reaction =
   SubMenu MenuID
 | Inactive
 -- | CallBack SomeHMACedThing
 deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Reaction

data MenuEntry =
  MEntry
  { _icon :: Maybe Text
  , _label :: Text
  , _reaction :: Reaction
  }
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeLenses ''MenuEntry

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
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''CompState

type MenuM a = StateT CompState IO a
