module Main where

import Alto.Example
import Alto.Web
import Alto.Menu
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  conf <- AltoConfig <$> exampleMenu <*> pure (ClientState mempty)
  run 8081 (altoApp conf)
