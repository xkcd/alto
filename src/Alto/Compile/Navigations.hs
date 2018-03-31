{-# LANGUAGE OverloadedStrings #-}
module Alto.Compile.Navigations where

import           Alto.Menu
import           Alto.Compile
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.ListTrie.Patricia.Map (TrieMap)
import qualified Data.ListTrie.Patricia.Map as LTP
import           Data.Map (Map)
import qualified Data.Text as T

-- | Makes a Patricia Trie into a set of actions.
--   Doesn't handle all Tries.
trieMenu :: [(String, EntryType)] -> MenuM Menu
trieMenu =
    go . LTP.fromList
  where
    go :: TrieMap Map Char EntryType -> MenuM Menu
    go = menu . breakPre
    breakPre :: TrieMap Map Char EntryType -> EntryM ()
    breakPre t = do
      iforM_ (LTP.children1 t) $ \fc st -> do
        case LTP.splitPrefix . LTP.addPrefix [fc] $ st of
          (pfx, Nothing, st') -> do
              sbmn <- lift $ go st'
              ent . MEntry Nothing (T.pack pfx) Always Always $ mnAction sbmn
          (pfx, Just a, st') -> do
              ent . MEntry Nothing (T.pack pfx) Always Always $ a
              unless (LTP.null st') $ do
                breakPre . LTP.addPrefix pfx $ st'

