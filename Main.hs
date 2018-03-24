module Main where

import Alto.Example
import Alto.Web
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  conf <- AltoConfig <$> exampleMenu
  run 8081 (altoApp conf)
