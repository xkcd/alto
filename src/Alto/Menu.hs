{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Alto.Menu where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.State (StateT)
import qualified Data.Aeson as JS
import qualified Data.Aeson.TH as JS
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lens
import           System.Directory (listDirectory)
import           System.FilePath
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

-- | Load a menu from a file
loadMenu :: FilePath -> IO Menu
loadMenu fp = do
  either error return =<< JS.eitherDecode' <$> BSL.readFile fp

-- | Loads a MenuSystem directory. The format is:
--   FP:
--     - root <- file containing root menu's ID
--     - menus/ <- directory of one file per menu
loadMenus :: FilePath -> IO MenuSystem
loadMenus fp = do
  rmID <- TIO.readFile $ fp </> "root"
  mns <- (fmap ((fp</>"menus")</>) <$> listDirectory (fp </> "menus")) >>=
         (fmap (Map.fromList . map (\a -> (a ^.mid, a))) . mapM loadMenu)
  maybe (error "Couldn't find root menu in MenuSystem!") return . fmap (MenuSystem mns) $
    mns  ^.at rmID

-- | Save a MenuSystem so it can be reloaded later for serving or use as a
--   subcomponent of another MenuSystem.
saveMenus :: FilePath -> MenuSystem -> IO ()
saveMenus fp ms = do
  TIO.writeFile (fp</>"root") (ms^.topMenu.mid)
  ifor_ (ms^.menuMap) $ \i m ->
    JS.encodeFile ((fp</>"menus")</>(T.unpack i)) m
