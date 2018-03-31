{-# LANGUAGE OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , FlexibleContexts
 #-}
module Alto.Compile where

import           Alto.Menu
import           Control.Lens
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA256
import           Crypto.Scrypt (ScryptParams)
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.RandomString

saltDerivingParams :: ScryptParams
saltDerivingParams = Scrypt.defaultParams

type MenuM a = StateT CompState IO a
type EntryM a = WriterT [MenuEntry] (StateT CompState IO) a

-- | Compiles a MenuSystem given a name we produce a salt from.
--   Any menu systems sharing tags must agree on the project name.
compileRoot :: Text -> MenuM Menu -> IO MenuSystem
compileRoot name desc = do
  let compSalt = B64.encode . Scrypt.getEncryptedPass . Scrypt.encryptPass
                 saltDerivingParams (Scrypt.Salt $ TE.encodeUtf8 "Jektulv!OCod3gob6Glaj@") .
                 Scrypt.Pass . TE.encodeUtf8 $ name
  (rm, (CSt _ mnmp _)) <- desc `runStateT` (CSt compSalt mempty mempty)
  return $ MenuSystem mnmp rm

-- | Generate a (hopefully) unique ID based off the name, pseudo-salted from the root name.
--   The root derived pseudo salt is expensively generated to make guessing attacks
--   fairly unreasonable. Truly though this is just to keep the honest honest.
genTagID :: MonadState CompState m => Text -> m Tag
genTagID nm = do
  ss <- use salt
  -- If our parts encode the same, we are the same.
  return . T.init . TE.decodeUtf8 . B64.encode . SHA256.hashlazy .
    BSL.fromChunks $ [TE.encodeUtf8 nm, ss]

genMenuID :: MenuM MenuID
genMenuID = lift $ randomString (StringOpts Base58 (256 `div` 8))

importMenuSystem :: MenuSystem -> MenuM Menu
importMenuSystem ms = do
  menus <>= (ms ^. menuMap)
  return (ms ^. topMenu)

-- | Import a menu system for use in this menu system.
--   Returns the root of said menu system.
importMenus :: FilePath -> MenuM Menu
importMenus fp = do
  ms <- lift $ loadMenus fp
  importMenuSystem ms

menu :: EntryM () -> MenuM Menu
menu ents = do
  es <- execWriterT ents
  cid <- genMenuID
  let mn = Menu cid es
  -- Make sure this ID isn't already in use.
  omns <- menus <<%= (Map.insert cid mn)
  when (cid `Map.member` omns) $
    error ("The menu "<>(show es)<>" was already in used!")
  return $ mn

uniqueTag :: MonadState CompState m => Text -> m Tag
uniqueTag t = do
  tid <- genTagID t
  existed <- use $ tags.contains tid
  when existed $ error "Tag already existed!"
  return tid

-- | Add an entry to the menu
ent :: MenuEntry -> EntryM ()
ent = tell . pure

mnAction :: Menu -> EntryType
mnAction m = SubMenu (m^.mid) Nothing mempty mempty

andLogic :: TagLogic -> TagLogic -> TagLogic
andLogic nl Always = nl
andLogic nl (TLAnd ol) = TLAnd $ nl:ol
andLogic nl ol = TLAnd [nl, ol]

-- | Require a tag to be set for a menu entry to be displayed.
infixl 5 &+
(&+) :: MenuEntry -> Tag -> MenuEntry
(&+) e t = e & display %~ andLogic (TagSet t)

-- | Require a tag to be unset for a menu entry to be displayed.
infixl 5 &-
(&-) :: MenuEntry -> Tag -> MenuEntry
(&-) e t = e & display %~ andLogic (TagUnset t)

-- | Requires a specific tag logic to be true.
infixl 5 &=
(&=) :: MenuEntry -> TagLogic -> MenuEntry
(&=) e tl = e & display %~ andLogic tl

-- Make a MenuEntry link to a submenu
-- (|-$) :: MenuEntry -> MenuM Menu -> MenuM MenuEntry

infixl 5 |->
(|->) :: MenuEntry -> Menu -> MenuEntry
(|->) e m = e & reaction .~ SubMenu (m ^. mid) Nothing (e ^. reaction.setTags) (e ^. reaction.unsetTags)

infixl 5 |-=
(|-=) :: MenuEntry -> Action -> MenuEntry
(|-=) e a = e & reaction .~ Action (e ^. reaction.setTags) (e ^. reaction.unsetTags) (Just a)

infixl 5 |-//
(|-//) :: MenuEntry -> Text -> MenuEntry
(|-//) e u = e |-= (Nav u)

-- | Make a MenuEntry set a tag.
infixl 5 |-+
(|-+) :: MenuEntry -> Tag -> MenuEntry
(|-+) e t = e & reaction.setTags <>~ (Map.singleton t "")

-- | Make a MenuEntry sset a number of tags.
infixl 5 |-+*
(|-+*) :: MenuEntry -> [Tag] -> MenuEntry
(|-+*) e t = e & reaction.setTags <>~ (Map.fromList . map (,"") $ t)

-- | Make a MenuEntry set a tag.
infixl 5 |-+=
(|-+=) :: MenuEntry -> Tag -> Text -> MenuEntry
(|-+=) e t v = e & reaction.setTags <>~ (Map.singleton t v)

-- | Make a MenuEntry unset a tag.
infixl 5 |--
(|--) :: MenuEntry -> Tag -> MenuEntry
(|--) e t = e & reaction.unsetTags <>~ (Set.singleton t)

-- | Make a MenuEntry unset a number of tags.
infixl 5 |--*
(|--*) :: MenuEntry -> [Tag] -> MenuEntry
(|--*) e t = e & reaction.unsetTags <>~ (Set.fromList t)

-- | Like |-> but generates where it links off the value of a tag.
infixl 5 |=>
(|=>) :: MenuEntry -> Text -> Tag -> MenuEntry
(|=>) e mpre tg = e & reaction .~ SubMenu mpre (Just tg)  (e ^. reaction.setTags) (e ^. reaction.unsetTags)
