{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Alto.Compile where

import           Alto.Types
import           Control.Lens
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA256
import           Crypto.Scrypt (ScryptParams, scryptParams)
import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as JS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

saltDerivingParams :: ScryptParams
saltDerivingParams = Scrypt.defaultParams

compileRoot :: Text -> MenuM Menu -> IO MenuSystem
compileRoot name desc = do
  let compSalt = B64.encode . Scrypt.getEncryptedPass . Scrypt.encryptPass
                 saltDerivingParams (Scrypt.Salt $ TE.encodeUtf8 "Jektulv!OCod3gob6Glaj@") .
                 Scrypt.Pass . TE.encodeUtf8 $ name
  (rm, (CSt _ mnmp _)) <- desc `runStateT` (CSt compSalt mempty mempty)
  return $ MenuSystem mnmp (GMS (ClientState mempty)) rm

-- | Generate a (hopefully) unique ID based off the name, pseudo-salted from the root name.
--   The root derived pseudo salt is expensively generated to make guessing attacks
--   fairly unreasonable. Truly though this is just to keep the honest honest.
genTagID :: Text -> MenuM Text
genTagID nm = do
  ss <- use salt
  -- If our parts encode the same, we are the same.
  return . TE.decodeUtf8 . B64.encode . SHA256.hashlazy .
    BSL.fromChunks $ [TE.encodeUtf8 nm, ss]


genMenuID :: [MenuEntry] -> MenuM MenuID
genMenuID me = do
  ss <- use salt
  return . TE.decodeUtf8 . B64.encode . SHA256.hashlazy . BSL.fromChunks .
    (`mappend` [ss]) . map (BSL.toStrict . JS.encode) $ me

type EntryM a = WriterT [MenuEntry] MenuM a

-- Checks that the 
menu :: EntryM () -> MenuM Menu
menu entries = do
  let es = execWriter entries
  cid <- genMenuID es
  let mn = Menu cid es
  -- Make sure this ID isn't already in use.
  omns <- menus <<%= (Map.insert cid mn)
  when (cid `Map.member` omns) $
    error ("The menu "<>(show es)<>" was already in used!")
  return $ mn

uniqueTag :: Text -> MenuM Tag
uniqueTag t = 

-- -- | Adds a basic menu entry to the currently building menu.
--entry :: Text -> Reaction -> Writer [MenuEntry] ()
--entry n r = tell . pure $ MEntry Nothing n r

-- | Make a MenuEntry set a tag.
(|+) :: MenuEntry -> Tag -> MenuM MenuEntry

-- | Make a MenuEntry unset a tag.
(|-)_ :: MenuEntry -> Tag -> MenuM MenuEntry

-- | Attach a display condition to a MenuEntry
(&-) :: MenuEntry -> Tag -> MenuM MenuEntry

-- | Make a MenuEntry link to a submenu
(|>) :: MenuEntry -> Menu -> MenuM MenuEntry
(|>) :: MenuEntry -> MenuM Menu -> MenuM MenuEntry

class MakeEntry t where
  ent :: t -> Writer [MenuEntry] ()

instance MakeEntry Text where
  ent l = tell . pure $ MEntry Nothing l Inactive Always

instance MakeEntry (Text -> 

linkMenu :: Menu -> Reaction
linkMenu = SubMenu . _mid
