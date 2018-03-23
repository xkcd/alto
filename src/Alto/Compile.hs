module Alto.Compile where

compileRoot :: Text -> GlobalMenuState -> MenuM Menu -> IO MenuSystem
compileRoot name desc =

-- Checks that the 
subMenu :: Text -> Writer [MenuEntry] () -> MenuM Menu
subMenu name icon entries = 

entry :: Text -> Reaction -> MenuEntry