module Alto.Types where

type MenuID = ()

type GlobalMenuState = ()

data Reaction =
   RJSAction JSFunction
 | SubMenu MenuID
 | NoAction

data MenuEntry =
  MEntry
  {  _icon :: Maybe URL
  , _text :: Text
  , _reaction :: Reaction
  }

data Menu =
  Menu
  { _mid :: MenuID
  , _entries :: [MenuEntry]
  }

data Root =
  MenuRoot
  { _rootState :: GlobalMenuState
  , _rootMenu :: Menu
  }

data MenuSystem =
  MenuSystem
  { _menuMap :: Map MenuID Menu
  , _globalState :: GlobalMenuState
  , _topMenu :: Menu
  }

type MenuM a = StateT (Map MenuID Menu) IO a
