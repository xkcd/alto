{-# LANGUAGE OverloadedStrings #-}
module Alto.Compile.Maze where

import           Alto.Compile
import           Alto.Menu
import           Control.Monad
import           Control.Monad.Fix
import           Data.String
import qualified Data.Text         as T
import           Data.Tree         (Tree(..))
import qualified Data.Tree         as Tree
import           Control.Monad.Random
import           Data.Bifunctor
import           Data.Tree.Lens
import           Control.Lens

type Seed = ()

type Maze key = Tree (MazeEntry key)

type Zipper = [Int]

data MazeEntry key =
    MazeEmpty
  | MazeLocked {-(T.Text, EntryType)-} key
  | MazeKey key
  deriving (Eq, Ord, Show)

data Complexity = Complexity {
  complexityBranchRange :: (Int, Int) -- ^ (min,max) number of branches
, complexityHiddenDistanceRange :: (Int, Int) -- ^ (min, max) range of the distance from a lock to a key
, complexityTreeDepth :: Int -- ^ (min, max) multipliers for the height, kind of weird
}

complexityNodeRange :: Complexity -> (Int, Int)
complexityNodeRange complexity = (max depth (minBranches ^ depth), maxBranches ^ depth)
  where
    (minBranches, maxBranches) = complexityBranchRange complexity
    depth = complexityTreeDepth complexity

generateMaze :: MonadRandom m => Complexity -> [(T.Text, EntryType)] -> m (Tree ())
generateMaze complexity hidden =
  return $ Node () []
  where
    hiddenItemsCount = length hidden

-- | generate an empty maze
generateMazeTree :: MonadRandom m => Complexity -> m (Maze key)
generateMazeTree complexity = do
  go 0
  where
    go depth
      | goalDepth > depth = do
        goalBranches <- getRandomR $ complexityBranchRange complexity
        Node MazeEmpty <$> (replicateM goalBranches $ go $ depth + 1)
      | goalDepth >= depth = return $ Node MazeEmpty []
      | otherwise = fail "depth issue in generateMazeTree"
    goalDepth = complexityTreeDepth complexity

runZipper :: Zipper -> Maze key -> Maze key
runZipper [] maze = maze
runZipper (i:is) (Node _ submazes) = runZipper is $ submazes !! i


modifyAtZipper :: Zipper -> (Tree a -> Tree a) -> Tree a -> Tree a
modifyAtZipper [] f t = f t
modifyAtZipper (i:is) f tree =
  tree
    & branches . ix i %~ (modifyAtZipper is f)

solveableMaze :: Eq key
              => Int -- ^ Number of times to iterate
              -> Maze key -- ^ Maze to check
              -> Bool
solveableMaze depth maze =
  all (== MazeEmpty) . (!! depth) $ iterate runThroughMaze maze
  where
    runThroughMaze m = unlockDoors (availableKeys m) m

availableKeys :: Maze key -> [key]
availableKeys maze =
  go maze
  where
    go (Node (MazeKey x) submazes) =
      x:(concat $ go <$> submazes)
    go (Node (MazeLocked _) _) =
      []
    go (Node MazeEmpty submazes) =
      concat $ go <$> submazes

unlockDoors :: Eq key => [key] -> Maze key -> Maze key
unlockDoors keys node@(Node (MazeLocked key) submazes)
  | key `elem` keys = Node MazeEmpty $ unlockDoors keys <$> submazes
  | otherwise = node
unlockDoors keys (Node _ submazes) =
  Node MazeEmpty $ unlockDoors keys <$> submazes

placeKeyAndDoor :: MonadRandom m
                => Complexity
                -> Int -- ^ Level to place the locked door in
                -> key -- ^ Key to place in the maze
                -> Maze key -- ^ maze to place the key in
                -> m (Maze key)
placeKeyAndDoor complexity level key maze = do
  pathToDoor <- findAcceptablePath level paths
  distance <- getRandomR distanceRange
  upDistance <- getRandomR (1, max (level - 1) distance) -- 2 just to be safe...
  let
    downDistance = distance - upDistance
    rootZipper = take (level - upDistance) pathToDoor
    rootMaze = runZipper rootZipper maze
    rootMazePaths = possiblePaths rootMaze
  pathToKeyFromRoot <- findAcceptablePath downDistance rootMazePaths
  let
    pathToKey = rootZipper ++ pathToKeyFromRoot
  return $
    maze
      & modifyAtZipper pathToKey addKey
      & modifyAtZipper pathToDoor addDoor
  where
    distanceRange = complexityHiddenDistanceRange complexity
    paths = possiblePaths maze
    findAcceptablePath l ps = do
      startIndex <- getRandomR (0, length ps - 1)
      let
        firstAcceptablePath i
          | length (ps !! i) >= l = take l $ ps !! i
          | otherwise = firstAcceptablePath (i + 1 `mod` (pathsLength - 1))
      return $ firstAcceptablePath startIndex
      where
        pathsLength = length ps

    addDoor maze = maze & root .~ MazeLocked key
    addKey maze = maze & root .~ MazeKey key

complexity = (Complexity (2,4) (3,6) 12)

-- | Lazily generate all of the possible paths in a
-- maze
possiblePaths :: Tree (MazeEntry key) -> [Zipper]
possiblePaths maze =
  reverse <$> go [] maze
  where
    go :: Zipper -> Tree (MazeEntry key) -> [Zipper]
    go zipper (Node (MazeLocked _) _) = [zipper]
    go zipper (Node _ []) =
      [zipper]
    go zipper (Node _ otherPaths) =
      concat $ map (\(path, index) -> go (index:zipper) path) $ zip otherPaths [0..]

-- generateMaze :: Seed -> Complexity -> [Text, EntryType)] -> IO MenuSystem
-- generateMaze seed complexity hidden = compileRoot "Example Menu" $ do
--   de <- menu $ ent "Dead end!"
--   menu $ do
--     ent "This is like a header"
--     ent $ "Sub Menu" |-> de
-- --    ent "A directly defined submenu" $ do
-- --      ent "This is an entry in a directly defined submenu."
--     -- A sometimes hidden menu that
--     hideIt <- uniqueTag "Hide it" -- Makes sure the tag isn't used by something else
--     ent $ "Hide me!" &- hideIt |-+ hideIt
--     ent $ "Unhide it" &+ hideIt |-- hideIt
--     ent "Final entry"
