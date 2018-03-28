module Alto.Compile.BitPuzzle where

-- | Takes a menu, and gates access to it on someone's ability to "unlock"
--   access through pushing switches that fip various bits.
--   Difficulty is based on number of bits, and number of flips done.
genBitLock :: Menu -> Int -> Int -> MenuM Menu
genBitLock m bitCnt flipCnt = do
  