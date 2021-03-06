{-# LANGUAGE OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , FlexibleContexts
 #-}
module Alto.Compile where

import           Alto.Menu
import           Control.Lens
import qualified Control.Monad.Catch as E
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA256
import           Crypto.Scrypt (ScryptParams)
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.RandomString

saltDerivingParams :: ScryptParams
saltDerivingParams = Scrypt.defaultParams

type MenuM a = StateT CompState IO a
type EntryM a = WriterT [MenuEntry] (StateT CompState IO) a

-- | Loads a subgraph if it exists, otherwise compiles it.
subGraph :: Text -> MenuM Menu -> MenuM Menu
subGraph sgName desc =
  E.catch (lift $ refSubGraph sgName) $ \(_::E.SomeException) -> do
    ms <- lift $ compileRoot sgName desc
    lift $ saveSubGraph sgName ms
    return . fromJust $ ms^.menuMap.at (ms^.topMenu.mid)

-- | Compiles a MenuSystem given a name we produce a salt from.
--   Any menu systems sharing tags must agree on the project name.
compileRoot :: Text -> MenuM Menu -> IO MenuSystem
compileRoot name desc = do
  let compSalt = B64.encode . Scrypt.getEncryptedPass . Scrypt.encryptPass
                 saltDerivingParams (Scrypt.Salt $ TE.encodeUtf8 "Jektulv!OCod3gob6Glaj@") .
                 Scrypt.Pass . TE.encodeUtf8 $ name
  (rm, (CSt _ mnmp _)) <- desc `runStateT` (CSt compSalt mempty mempty)
  return $ MenuSystem mnmp rm

idBytes :: Int
idBytes = 128 `div` 8

-- | Generate a (hopefully) unique ID based off the name, pseudo-salted from the root name.
--   The root derived pseudo salt is expensively generated to make guessing attacks
--   fairly unreasonable. Truly though this is just to keep the honest honest.
genTagID :: MonadState CompState m => Text -> m Tag
genTagID nm = do
  ss <- use salt
  -- If our parts encode the same, we are the same.
  return . T.init . TE.decodeUtf8 . B64.encode . BS.take idBytes .
    SHA256.hashlazy .
    BSL.fromChunks $ [TE.encodeUtf8 nm, ss]

genMenuID :: MenuM MenuID
genMenuID = lift $ randomString (StringOpts Base58 idBytes)

-- | Import a menu system for use in this menu system.
--   Returns the root of said menu system.
importMenuSystem :: MenuSystem -> MenuM Menu
importMenuSystem ms = do
  menus <>= (ms ^. menuMap)
  return (ms ^. topMenu)

runEntryM :: EntryM () -> MenuM [MenuEntry]
runEntryM = execWriterT

updateEntries :: Menu -> EntryM () -> MenuM ()
updateEntries m entry =
  void $ updateEntries' m entry

updateEntries' :: Menu -> EntryM () -> MenuM Menu
updateEntries' m entry = do
  ents <- runEntryM entry
  let
    m' = m & entries <>~ ents
  updateMenu m'
  return m'

menu' :: TagChange -> EntryM () -> MenuM Menu
menu' exitTC ents = do
  es <- runEntryM ents
  cid <- genMenuID
  let mn = Menu cid exitTC es
  -- Make sure this ID isn't already in use.
  omns <- menus <<%= (Map.insert cid mn)
  when (cid `Map.member` omns) $
    error ("The menu "<>(show es)<>" was already in used!")
  return $ mn

menu :: EntryM () -> MenuM Menu
menu = menu' mempty

updateMenu :: Menu -> MenuM ()
updateMenu m =
  menus #%= (Map.insert (m ^. mid) m)

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
mnAction m = SubMenu mempty (m^.mid) Nothing

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
(|->) e m = e & reaction .~ SubMenu (e^.reaction.onAction) (m ^. mid) Nothing

infixl 5 |-=
(|-=) :: MenuEntry -> Action -> MenuEntry
(|-=) e a = e & reaction .~ Action (e^.reaction.onAction) (Just a)

infixl 5 |-==
(|-==) :: MenuEntry -> EntryType -> MenuEntry
(|-==) e a = e & reaction .~ (a&onAction.~(e^.reaction.onAction))

infixl 5 |-//
(|-//) :: MenuEntry -> Text -> MenuEntry
(|-//) e u = e |-= (Nav u)

infixl 5 |-#
(|-#) :: MenuEntry -> Text -> EmbedSize -> MenuEntry
(|-#) e u es = e |-= (Embed u es)

-- | Make a MenuEntry set a tag.
infixl 5 |-+
(|-+) :: MenuEntry -> Tag -> MenuEntry
(|-+) e t = e & reaction.onAction.setTags <>~ (Map.singleton t "")

-- | Make a MenuEntry sset a number of tags.
infixl 5 |-+*
(|-+*) :: MenuEntry -> [Tag] -> MenuEntry
(|-+*) e t = e & reaction.onAction.setTags <>~ (Map.fromList . map (,"") $ t)

-- | Make a MenuEntry set a tag.
infixl 5 |-+=
(|-+=) :: MenuEntry -> Tag -> Text -> MenuEntry
(|-+=) e t v = e & reaction.onAction.setTags <>~ (Map.singleton t v)

infixl 5 |-<>
(|-<>) :: MenuEntry -> TagChange -> MenuEntry
(|-<>) e tc = e & reaction.onAction <>~ tc

-- | Make a MenuEntry unset a tag.
infixl 5 |--
(|--) :: MenuEntry -> Tag -> MenuEntry
(|--) e t = e & reaction.onAction.unsetTags <>~ (Set.singleton t)

-- | Make a MenuEntry unset a number of tags.
infixl 5 |--*
(|--*) :: MenuEntry -> [Tag] -> MenuEntry
(|--*) e t = e & reaction.onAction.unsetTags <>~ (Set.fromList t)

-- | Like |-> but generates where it links off the value of a tag.
infixl 5 |=>
(|=>) :: MenuEntry -> Text -> Tag -> MenuEntry
(|=>) e mpre tg = e & reaction .~ SubMenu (e^.reaction.onAction) mpre (Just tg)
