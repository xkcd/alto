{-# LANGUAGE TemplateHaskell #-}
module Alto.Types where

import Control.Lens.TH
import Control.Monad.State (StateT)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)

type MenuID = Text

type ClientState = ()

data GlobalMenuState =
  GMS
  { _clientState :: ClientState
  , _privateState :: ()
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''GlobalMenuState

data Reaction =
   SubMenu MenuID
 | Inactive
 deriving (Read, Show, Eq, Ord)

makeLenses ''Reaction

data MenuEntry =
  MEntry
  { _icon :: Maybe Text
  , _label :: Text
  , _reaction :: Reaction
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''MenuEntry

data Menu =
  Menu
  { _mid :: MenuID
  , _entries :: [MenuEntry]
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Menu

data Root =
  MenuRoot
  { _rootState :: ClientState
  , _rootMenu :: Menu
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''Root

data MenuSystem =
  MenuSystem
  { _menuMap :: Map MenuID Menu
  , _globalState :: GlobalMenuState
  , _topMenu :: Menu
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''MenuSystem

data CompState =
  CSt
  { _salt :: ByteString
    -- ^ A pseudo-salt derived expensively from the overall name. 
  , _menus :: Map MenuID Menu
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''CompState

type MenuM a = StateT CompState IO a
