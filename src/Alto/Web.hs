{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Alto.Web where

import           Alto.Menu
import           Control.Lens
import           Data.Text (Text)
import           Servant

data AltoConfig =
  AltoConfig
  { _mSys :: MenuSystem
  , _initState :: ClientState
  }
  deriving (Read, Show, Eq, Ord)

makeLenses ''AltoConfig

type RootAPI = "root" :> Get '[JSON] Root
type MenuAPI = "menu" :> Capture "menuid" MenuID :> Get '[JSON] (Headers '[Header "Cache-Control" Text] Menu)
-- type CallbackAPI = "callback" :> ReqBody '[PlainText] ByteString :> Post '[JSON] Event

type API = RootAPI :<|> MenuAPI

api :: Proxy API
api = Proxy

serveRoot :: AltoConfig -> Handler Root
serveRoot cfg = return $ MenuRoot (cfg^.initState) (cfg^.mSys.topMenu)

serveMenu :: AltoConfig -> Text -> Handler Menu
serveMenu cfg i =
  case cfg ^. mSys.menuMap.at i of
    Nothing -> throwError err404
    Just m -> return m

addCacheHeader :: Handler Menu -> Handler (Headers '[Header "Cache-Control" Text] Menu)
addCacheHeader = fmap (addHeader "public, max-age=1")

altoServer :: AltoConfig -> Server API
altoServer cfg = serveRoot cfg :<|> (addCacheHeader <$> serveMenu cfg)

altoApp :: AltoConfig -> Application
altoApp c = serve api (altoServer c)