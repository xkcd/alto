{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Alto.Web where

import           Alto.Types
import           Control.Lens
import           Control.Lens.TH
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Text (Text)
--import           Network.Wai
import           Servant

data AltoConfig =
  AltoConfig
  { _mSys :: MenuSystem
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''AltoConfig

type RootAPI = "root" :> Get '[JSON] Root
type MenuAPI = "menu" :> Capture "menuid" MenuID :> Get '[JSON] Menu
-- type CallbackAPI = "callback" :> ReqBody '[PlainText] ByteString :> Post '[JSON] Event

type API = RootAPI :<|> MenuAPI

api :: Proxy API
api = Proxy

serveRoot :: AltoConfig -> Handler Root
serveRoot cfg = return $ MenuRoot (cfg ^. mSys.globalState.clientState) (cfg ^. mSys.topMenu)

serveMenu :: AltoConfig -> Text -> Handler Menu
serveMenu cfg i =
  case cfg ^. mSys.menuMap.at i of
    Nothing -> throwError err404
    Just m -> return m

altoServer :: AltoConfig -> Server API
altoServer cfg = serveRoot cfg :<|> serveMenu cfg 

altoApp :: AltoConfig -> Application
altoApp c = serve api (altoServer c)