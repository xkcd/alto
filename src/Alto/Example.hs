module Alto.Example where

exampleMenu :: IO MenuSystem
exampleMenu = compileRoot "Example Menu" () $ do
  subA     <- subMenu "sub menu A" $ do
                entry "This is a menu option" jsCloseSite
                entry "This is recursive" subA
  mazeS    <- subMenu "Maze S" $
                return ()
  mazeN    <- subMenu "Maze N" $ do
                entry "You win!" NoAction
  mazeMenu <- subMenu "Maze" $ do
                entry "North" mazeN
                entry "South" mazeS
  subMenu "Top Level" $ do
    entry "Maze" mazeMenu
    entry "Sub menu" subA
