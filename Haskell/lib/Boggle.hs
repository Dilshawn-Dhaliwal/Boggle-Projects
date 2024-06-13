module Boggle (boggle) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.List as L

-- USING A TRIE (PREFIX TREE) DATA STRUCTURE
-- TrieEdges represent a Map. The key is the current char, Values are the next char(s) in the Prefix Tree
type TrieEdges a = M.Map a (Trie a)

-- The Trie is made up of TrieEdges. EndNode indicates that the node is the end of a word.
data Trie a =
  Node { edges :: TrieEdges a }
  | EndNode { edges :: TrieEdges a }
  deriving (Show, Eq)

-- initializes an Empty Prefix Tree
emptyTrie :: Trie a
emptyTrie = Node M.empty

--Initalize, from a list of strings, a Trie of the strings.
initTrie :: Ord a => [[a]] -> Trie a -> Trie a
initTrie words trie = foldl (flip insert) trie words

-- Inserts a word into the Prefix Tree
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] n = EndNode (edges n)
insert ws (Node m) = Node (insertBelow ws m)
insert ws (EndNode m) = EndNode (insertBelow ws m)

-- Helper function for above function, used to insert word below the current node
insertBelow :: Ord a => [a] -> Map a (Trie a) -> Map a (Trie a)
insertBelow (c:rest) m = M.insert c (insert rest child) m
  where child = M.findWithDefault emptyTrie c m

-- Check if a word is a member or prefix in the Prefix Tree
-- Logic of checking for membership and a valid prefix is the same, so use a pair tuple to reduce redundency.
-- Returns (isMember, isPrefix)
member :: Ord a => [a] -> Trie a -> (Bool, Bool)
member [] (EndNode _) = (True, True)
member [] (Node _) = (False, True)
member (c:rest) n = maybe (False, False) (member rest) (M.lookup c (edges n))



-- CHARACTER ADJACENCY MAP
-- For each character of the board, make a map of characters adjacent to that character.
-- Keys are the character, values are a set of adjacent characters.
-- Uses < since i'm adding +1 to x and/or y in the recursive calls
initMap :: Ord a => [[a]] -> Int -> Int -> Map a (Set a) -> Map a (Set a)
initMap board x y map
   -- While X is not at the end, Add coord Adjs to map and increment X
  | x<length board-1 = initMap board (x+1) y (addAdj board x y map) 
   -- When X is at the end and While Y is not at the end
  | x==length board-1 && y<length board-1 = initMap board 0 (y+1) (addAdj board x y map)
   -- Otherwise add the final adjacencies and return the finished adjMap
  | otherwise = addAdj board x y map 

-- Add adjacencies to the adjMap.
addAdj :: Ord a => [[a]] -> Int -> Int -> Map a (Set a) -> Map a (Set a)
addAdj board x y map

     -- Top-Left Corner
    | x-1<0 && y-1<0 = do 
        let adjList = S.fromList [rightAdj, botRightAdj, botAdj]
        M.insertWith S.union (board!!y!!x) adjList map
     
     -- Top-Right Corner
    | y-1<0 && x+1==length board = do 
        let adjList = S.fromList [leftAdj, botLeftAdj, botAdj]
        M.insertWith S.union (board!!y!!x) adjList map

     -- Bottom-Left Corner
    | x-1<0 && y+1==length board = do 
        let adjList = S.fromList [rightAdj, topRightAdj, topAdj]
        M.insertWith S.union (board!!y!!x) adjList map
        
     -- Bottom-Right Corner
    | x+1==length board && y+1==length board = do 
        let adjList = S.fromList [leftAdj, topLeftAdj, topAdj]
        M.insertWith S.union (board!!y!!x) adjList map

     -- Top Edge of Board
    | y-1<0 = do 
      let adjList = S.fromList [rightAdj, botRightAdj, botAdj, botLeftAdj, leftAdj]
      M.insertWith S.union (board!!y!!x) adjList map

     -- Left Edge of Board
    | x-1<0 = do 
      let adjList = S.fromList [topAdj, topRightAdj, rightAdj, botRightAdj, botAdj]
      M.insertWith S.union (board!!y!!x) adjList map

     -- Right Edge of Board
    | x+1==length board = do
      let adjList = S.fromList [topAdj, topLeftAdj, leftAdj, botLeftAdj, botAdj]
      M.insertWith S.union (board!!y!!x) adjList map

      -- Bottom Edge of Board
    | y+1==length board = do
      let adjList = S.fromList [rightAdj, topRightAdj, topAdj, topLeftAdj, leftAdj]
      M.insertWith S.union (board!!y!!x) adjList map

      -- Non-Edge. Add all adjacencies
    | otherwise = do
      let adjList = S.fromList [botLeftAdj, botAdj, botRightAdj, rightAdj, topRightAdj, topAdj, topLeftAdj, leftAdj]
      M.insertWith S.union (board!!y!!x) adjList map

     -- Each adjacent character. Thanks to lazy evaluation, I don't need to worry about out-of-bounds index errors.
    where rightAdj = board!!y!!(x+1)
          botRightAdj = board!!(y+1)!!(x+1)
          botAdj = board!!(y+1)!!x
          botLeftAdj = board!!(y+1)!!(x-1)
          leftAdj = board!!y!!(x-1)
          topLeftAdj = board!!(y-1)!!(x-1)
          topAdj = board!!(y-1)!!x
          topRightAdj = board!!(y-1)!!(x+1)



-- COORDINATE ADJACENCY MAP: 
-- Keys: Coordinate (y, x),  Values: List of Coords adjacent to it [(y',x')...]
-- A map of each coord and which ones are adjacent to it. Used for the Depth-First-Search
initCoord :: Ord a => [[a]] -> Int -> Int -> Map (Int, Int) [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
initCoord board x y coordMap
    -- While X is not at the end, Add coord Adjs to map and increment X
  | x<length board - 1 = initCoord board (x+1) y (addCoord board x y coordMap) 
    -- When X is at the end and While Y is not at the end
  | x==length board - 1 && y<length board - 1 = initCoord board 0 (y+1) (addCoord board x y coordMap) 
    -- Otherwise (the final coord, bottom right), return the finished adjMap
  | otherwise = addCoord board x y coordMap 

-- Helper function: Adds a coord, and its adjacencies to the Coord Adjacency Map
addCoord :: Ord a => [[a]] -> Int -> Int -> Map (Int, Int) [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
addCoord board x y coordMap

      -- Top-Left Corner
    | x-1<0 && y-1<0 = do
        let adjList = [rightAdj, botRightAdj, botAdj]
        M.insertWith (++) (y,x) adjList coordMap

      -- Top-Right Corner
    | y-1<0 && x+1==length board = do
        let adjList = [leftAdj, botLeftAdj, botAdj]
        M.insertWith (++) (y,x) adjList coordMap
      
      -- Bottom-Left Corner
    | x-1<0 && y+1==length board = do
        let adjList = [rightAdj, topRightAdj, topAdj]
        M.insertWith (++) (y,x) adjList coordMap

     -- Bottom-Right Corner
    | x+1==length board && y+1==length board = do
        let adjList = [leftAdj, topLeftAdj, topAdj]
        M.insertWith (++) (y,x) adjList coordMap

      -- Top Edge of Board
    | y-1<0 = do
      let adjList = [rightAdj, botRightAdj, botAdj, botLeftAdj, leftAdj]
      M.insertWith (++) (y,x) adjList coordMap
    
      -- Left Edge of Board
    | x-1<0 = do
      let adjList = [topAdj, topRightAdj, rightAdj, botRightAdj, botAdj]
      M.insertWith (++) (y,x) adjList coordMap

      -- Right Edge of Board
    | x+1==length board = do
      let adjList = [topAdj, topLeftAdj, leftAdj, botLeftAdj, botAdj]
      M.insertWith (++) (y,x) adjList coordMap

     -- Bottom Edge of Board
    | y+1==length board = do
      let adjList = [rightAdj, topRightAdj, topAdj, topLeftAdj, leftAdj]
      M.insertWith (++) (y,x) adjList coordMap
    
      -- Non-Edge. Add all adjacencies
    | otherwise = do
      let adjList = [botLeftAdj, botAdj, botRightAdj, rightAdj, topRightAdj, topAdj, topLeftAdj, leftAdj]
      M.insertWith (++) (y,x) adjList coordMap

    where rightAdj = (y,x+1)
          botRightAdj = (y+1,x+1)
          botAdj = (y+1,x)
          botLeftAdj = (y+1,x-1)
          leftAdj = (y,x-1)
          topLeftAdj = (y-1,x-1)
          topAdj = (y-1,x)
          topRightAdj = (y-1,x+1)

{-- Function to check if a single Word can Potentially* be made from the Charcter Adjacency Map
* This doesn't check for unique coordinates. 
  Example: the word "stat" cannot be made from board ["ea", 
                                                      "st"] 
           without reusing 't', but this function would say it is possible to make. --}
checkWord :: Ord a => [a] -> Map a (Set a) -> Bool
-- Base Case 1: If word is 1 char long, check if it's a valid key in the Character Map.
checkWord [c] map = M.member c map
-- Base Case 2: If word is 2 chars long, check if first char is a key, and that the values contain second char.
checkWord [c, v] map = M.member c map && isValue c v map
-- Recursion: Check first char has a key, and if those values contain the second char.
checkWord (c:v:rest) map =  M.member c map && isValue c v map && checkWord rest map

-- Helper function to check if a char is in the values set of the earlier char. 
isValue :: Ord a => a -> a -> Map a (Set a) -> Bool
isValue k v map
  | S.null vals = False -- If set is the empty set, return False. Transition cannot occur.
  | S.member v vals = True -- If it's a member, return True.
  | otherwise = False -- If set is not null, but elem wasn't a member, return false.
  where vals = M.findWithDefault S.empty k map -- Use empty set as default if char not found.

-- PRUNING ALGORITHM TO REMOVE INVALID WORDS FROM WORDLIST:
-- For each word in word list:
 -- If word can be made from character adjacency map, keep it.
  -- Otherwise, Remove it.
pruneWords :: Ord a => [[a]] -> Map a (Set a) -> Int -> [[a]]
pruneWords [word] map boardLen -- Base Case: Only a single word in word list
 | checkWord word map = [word]
 | otherwise = []
pruneWords (word:rest) map boardLen -- Recursion: More than 1 word.
 | checkWord word map = word:pruneWords rest map boardLen
 | otherwise = pruneWords rest map boardLen



{--
DEPTH-FIRST-SEARCH. 
Variables:
 board: The game board. A list of strings.
 coordMap: Coordinate adjacency Map. Map of coords (y, x) and their adjacencies [(y,x), ...]
 x, y: current x and y position in the board.
 curWord: Current word made from the DFS.
 prev: Previously visited coordinates.
 res: List of (word, [coords]) pair-tuples.
 trie: The Prefix Tree
--}
findWords :: Ord a => [[a]] -> Map (Int, Int) [(Int, Int)] -> Int -> Int -> [a] -> [(Int, Int)] -> [([a], [(Int, Int)])] -> Trie a -> [([a], [(Int, Int)])]
findWords board coordMap x y curWord prev res trie = do
  let newPrev = (y,x):prev  -- Visit current coord -- ++ [(y,x)]
      newWord = curWord ++ [(board!!y)!!x] -- Assemble a word from board
      (isMember, prefix) = member newWord trie -- Check if newly assembled word is a member and/or valid prefix in Trie
      newRes = if isMember && not (isDupe newWord res) then noDupe ((newWord, reverse newPrev):res) else res
          -- If current word is in the wordTrie, & is not a duplicate word then prepend the pair-tuple to result. Otherwise, keep res the same
  if not prefix then newRes -- If current word is not a prefix, then don't go further. Return the results
  else do 
    let children = M.findWithDefault [] (y,x) coordMap -- Children of the current coord in the map.
    findChildWords children board coordMap newWord newPrev newRes trie -- Apply the findWords func to the children

-- Helper function for findWords: For each child provided to it, apply the findWords function
findChildWords :: Ord a => [(Int, Int)] -> [[a]] -> Map (Int, Int) [(Int, Int)]  -> [a] -> [(Int, Int)] -> [([a], [(Int, Int)])] -> Trie a -> [([a], [(Int, Int)])]
findChildWords [] _ _ _ _ res _ = res-- Base, no children.
findChildWords (current:rest) board coordMap curWord prev res trie = do
  let childX = snd current
  let childY = fst current
  -- If child not in prev, do it for this child, otherwise, skip it.
  if (childY,childX) `notElem` prev
    then findWords board coordMap childX childY curWord prev res trie ++ findChildWords rest board coordMap curWord prev res trie
    else findChildWords rest board coordMap curWord prev res trie

-- Applies findWords to all coordinates in the board.
findAllWords :: Ord a => [[a]] -> Map (Int, Int) [(Int, Int)] -> Int -> Int -> Trie a -> [([a], [(Int, Int)])] -> [([a], [(Int, Int)])]
findAllWords board coordMap x y trie res -- For each coord, do the DFS (findWords)
    -- If X not at the end
  | x < length board - 1 =
    findAllWords board coordMap (x+1) y trie (noDupe (res ++ findWords board coordMap x y [] [] [] trie))
   -- If X is at the end, but Y isnt
  | x==length board - 1 && y < length board - 1 =
    findAllWords board coordMap 0 (y+1) trie (noDupe (res ++ findWords board coordMap x y [] [] [] trie))
   -- Otherwise, it's the final coordinate
  | otherwise =
    noDupe (res ++ findWords board coordMap x y [] [] [] trie)

-- Helper function to check if a word is a duplicate in the results list
isDupe :: Ord a => [a] -> [([a], [(Int, Int)])] -> Bool
isDupe _ [] = False -- Base Case: If there's nothing in result, there's no dupe
isDupe word [curRes] = do fst curRes == word -- Base Case: If single elem in result, check if they match
isDupe word (curRes:rest) = do (fst curRes == word) || isDupe word rest -- Recursive Case.

-- Helper function to remove duplicate pair-tuples in the result.
noDupe :: Ord a => [([a], [(Int, Int)])] -> [([a], [(Int, Int)])]
noDupe res = S.toList (S.fromList res)



-- Putting it all together. 
boggle :: [String] -> [String] -> [ (String, [ (Int, Int) ] ) ]
boggle board words = do
  let adjMap = initMap board 0 0 M.empty -- Adjacency map of each character. 
      coordMap = initCoord board 0 0 M.empty -- Map of Coords, and their Adjacencies.
      boardSize = length board * length board -- # of chars in the board. 
      newWords = pruneWords words adjMap boardSize -- New list of words, now pruned. 
      wordTrie = initTrie newWords emptyTrie -- Trie of pruned words.

  removeDuplicates (findAllWords board coordMap 0 0 wordTrie []) -- Final Result

 -- Ensure there's no duplicate words in the list of pair tuples.
cmpKey :: Eq a => (a, b1) -> (a, b2) -> Bool
cmpKey (x, _) (x', _) = x == x'

 -- Removes duplicate words in the final word list using cmpKey.
removeDuplicates :: [(String, b1)] -> [(String, b1)]
removeDuplicates = L.nubBy cmpKey
