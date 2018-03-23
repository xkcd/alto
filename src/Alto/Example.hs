{-# LANGUAGE OverloadedStrings #-}
module Alto.Example where

import Alto.Compile
import Alto.Types

exampleMenu :: IO MenuSystem
exampleMenu = compileRoot "Example Menu" (GMS () ()) $ do
  subA     <- subMenu "sub menu A" $ do
                entry "This is a menu option" Inactive
                entry "This is not recursive" Inactive
  mazeS    <- subMenu "Maze S" $
                return ()
  mazeN    <- subMenu "Maze N" $ do
                entry "You win!" Inactive
  mazeMenu <- subMenu "Maze" $ do
                entry "North" (linkMenu mazeN)
                entry "South" (linkMenu mazeS)
  subMenu "Top Level" $ do
    entry "Maze" (linkMenu mazeMenu)
    entry "Sub menu" (linkMenu subA)
