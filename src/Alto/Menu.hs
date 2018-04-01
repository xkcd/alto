{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Alto.Menu where

import           Control.Lens
import qualified Control.Monad.Catch as E
import qualified Data.Aeson as JS
import qualified Data.Aeson.TH as JS
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory (listDirectory, createDirectory)
import           System.FilePath
import           GHC.Generics

type MenuID = Text

type Tag = Text

data ClientState =
  ClientState
  { _clientTags :: Map Tag Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''ClientState
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 7, JS.constructorTagModifier = map toLower} ''ClientState

data TagLogic =
   Always
 | TagSet Tag
 | TagUnset Tag
 | TLAnd [TagLogic]
 | TLOr [TagLogic]
 | TLNot TagLogic
 deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TagChange =
  TagChange
  { _setTags :: Map Tag Text
  , _unsetTags :: Set Tag
  }
 deriving (Read, Show, Eq, Ord, Generic)

instance Semigroup TagChange where
  (<>) (TagChange a1 a2) (TagChange b1 b2) = TagChange (a1 <> b1) (a2 <> b2)

instance Monoid TagChange where
  mempty = TagChange mempty mempty

makeLenses ''TagChange
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1, JS.sumEncoding = JS.UntaggedValue} ''TagChange

data EmbedSize =
   EFullPage
 | ENative
 | ESize { _x :: Int, _y :: Int }
 deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''EmbedSize
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1} ''EmbedSize

data Action =
   ColapseMenu
 | Nav { _url :: Text }
 | Embed { _url :: Text, _size :: EmbedSize }
 | Download { _url :: Text, _filename :: Text }
 deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''Action
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1} ''Action

data EntryType =
   Action { _onAction :: TagChange, _act :: Maybe Action }
   -- ^ When the entry is clicked it does the above
 | SubMenu { _onAction :: TagChange, _subMenu :: MenuID, _subIdPostfix :: Maybe Tag }
   -- ^ When the entry is selected, the submenu is displayed
 -- | CallBack SomeHMACedThing
 | JSCall { _onAction :: TagChange, _jsCall :: Text }
 deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''EntryType
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1} ''EntryType

data MenuEntry =
  MEntry
  { _icon :: Maybe Text
  , _label :: Text
  , _display :: TagLogic
  , _active :: TagLogic
  , _reaction :: EntryType
  }
  deriving (Read, Show, Eq, Ord, Generic)

makeLenses ''MenuEntry
JS.deriveJSON JS.defaultOptions{JS.fieldLabelModifier = drop 1} ''MenuEntry

instance IsString MenuEntry where
  fromString l = MEntry Nothing (T.pack l) Always Always (Action mempty Nothing)

data Menu =
  Menu
  { _mid :: MenuID
  , _onLeave :: TagChange
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

existDirectory :: FilePath -> IO ()
existDirectory fp = E.catch (createDirectory fp) (\(_::E.SomeException) -> return ())

-- | Load a menu from a file
loadMenu :: FilePath -> IO Menu
loadMenu fp = do
  either error return =<< JS.eitherDecodeFileStrict' fp

-- | Loads a MenuSystem directory. The format is:
--   FP:
--     - root <- file containing root menu's ID
--     - menus/ <- directory of one file per menu
loadMenus :: IO MenuSystem
loadMenus = do
  root <- either error return =<< JS.eitherDecodeFileStrict' ("graph" </> "root")
  mns <- (fmap (("graph"</>"menu")</>) <$> listDirectory ("graph" </> "menu")) >>=
         (fmap (Map.fromList . map (\a -> (a ^.mid, a))) . mapM loadMenu)
  return . MenuSystem mns $ root^.rootMenu

-- | Save a MenuSystem so it can be reloaded later for serving or use as a
--   subcomponent of another MenuSystem.
saveMenus :: MenuSystem -> IO ()
saveMenus ms = do
  existDirectory "graph"
  JS.encodeFile ("graph"</>"root") . MenuRoot (ClientState mempty) $ ms^.topMenu
  storeSubMenus ms

storeSubMenus :: MenuSystem -> IO ()
storeSubMenus ms = do
  existDirectory $ "graph" </> "menu"
  ifor_ (ms^.menuMap) $ \i m ->
    JS.encodeFile (("graph"</>"menu")</>(T.unpack i)) m

saveSubGraph :: Text -> MenuSystem -> IO ()
saveSubGraph subname ms = do
  existDirectory "graph"
  storeSubMenus ms
  existDirectory $ "graph" </> "subgraph"
  TIO.writeFile (("graph"</>"subgraph")</>(T.unpack subname)) (ms^.topMenu.mid)

refSubGraph :: Text -> IO Menu
refSubGraph subname = do
  mnId <- TIO.readFile (("graph"</>"subgraph")</>(T.unpack subname))
  loadMenu $ ("graph"</>"menu")</>(T.unpack mnId)
