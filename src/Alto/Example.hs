{-# LANGUAGE OverloadedStrings #-}
module Alto.Example where

import Alto.Compile
import Alto.Types

exampleMenu :: IO MenuSystem
exampleMenu = compileRoot "Example Menu" (GMS () ()) $ do
  de <- menu $ ent "Dead end!"
  menu $ do
    ent "This is like a header"
    ent "Sub Menu" |-> de
    ent "A directly defined submenu" |-> menu $ do
      ent "This is an entry in a directly defined submenu."
    -- A sometimes hidden menu that
    hideIt <- uniqueTag "Hide it" -- Makes sure the tag isn't used by something else
    ent "Hide me!" &- unset hideIt |-+ hideIt
    ent "Unhide it" &- set hideIt |-- hideIt
