{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Alto.Compile where

import           Alto.Types
import           Control.Lens
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA256
import           Crypto.Scrypt (ScryptParams, scryptParams)
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

saltDerivingParams :: ScryptParams
saltDerivingParams = Scrypt.defaultParams

compileRoot :: Text -> GlobalMenuState -> MenuM Menu -> IO MenuSystem
compileRoot name gst desc = do
  let compSalt = B64.encode . Scrypt.getEncryptedPass . Scrypt.encryptPass
                 saltDerivingParams (Scrypt.Salt $ TE.encodeUtf8 "Jektulv!OCod3gob6Glaj@") .
                 Scrypt.Pass . TE.encodeUtf8 $ name
  (rm, (CSt _ mnmp)) <- desc `runStateT` (CSt compSalt mempty)
  return $ MenuSystem mnmp gst rm

-- | Generate a (hopefully) unique ID based off the name, pseudo-salted from the root name.
--   The root derived pseudo salt is expensively generated to make guessing attacks
--   fairly unreasonable. Truly though this is just to keep the honest honest.
genMenuID :: Text -> MenuM MenuID
genMenuID nm = do
  ss <- use salt
  return . TE.decodeUtf8 . B64.encode . SHA256.hashlazy . BSL.fromChunks $ [TE.encodeUtf8 nm, ss]

-- Checks that the 
subMenu :: Text -> Writer [MenuEntry] () -> MenuM Menu
subMenu name entries = do
  cid <- genMenuID name
  let mn = Menu cid (execWriter entries)
  -- Make sure this ID isn't already in use.
  omns <- menus <%= (Map.insert cid mn)
  when (cid `Map.member` omns) $
    error ("The name "<>(T.unpack name)<>" was already in used!")
  return $ mn

-- | Adds a basic menu entry to the currently building menu.
entry :: Text -> Reaction -> Writer [MenuEntry] ()
entry n r = tell . pure $ MEntry Nothing n r

linkMenu :: Menu -> Reaction
linkMenu = SubMenu . _mid
