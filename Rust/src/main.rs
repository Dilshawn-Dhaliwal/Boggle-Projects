#![allow(non_snake_case, non_camel_case_types, dead_code)]

use std::collections::HashMap;
use std::collections::HashSet;

// TRIE NODES. contain a character, a bool if it's the end of a word, and a map of Trie children characters.
pub struct TrieNode {
    character: char,                   // Character of the Node
    is_word: bool,                     // Boolean if this character is the end of a Word
    children: HashMap<char, TrieNode>, // Map of the character and their children TrieNodes
}

// TRIENODE FUNCTIONS
impl TrieNode {
    // Make a root TrieNode. Used by makeTrie.
    pub fn makeRootNode() -> TrieNode {
        // Root nodes have character value of '_', since that is not a char in the Board.
        TrieNode {
            character: '_',
            is_word: false,
            children: HashMap::new(),
        }
    }

    // Make a TrieNode. Params are its character and a bool if it's the end of a word.
    pub fn makeNode(c: char, isWord: bool) -> TrieNode {
        TrieNode {
            character: c,
            is_word: isWord,
            children: HashMap::new(),
        }
    }

    // Function to insert a character into this node's children.
    // Params: c: The character to insert,  isWord: If the character is the end of a word
    pub fn insert_below(&mut self, c: char, isWord: bool) {
        self.children.insert(c, TrieNode::makeNode(c, isWord));
    }
}

// TRIE DATA STRUCTURE. Contains a root node, which is connected to other nodes.
pub struct Trie {
    rootNode: TrieNode,
}

// TRIE FUNCTIONS
impl Trie {
    // Make a new Trie with a root node.
    pub fn makeTrie() -> Trie {
        Trie {
            rootNode: TrieNode::makeRootNode(),
        }
    }

    pub fn insert(&mut self, word: &str) {
        let chars: Vec<char> = word.chars().collect(); // Make a vector of words characters
        let mut curNode = &mut self.rootNode; // Current node is the Root Node of the current Trie
        let mut lastIndex = 0; // Variable to check if we finish the word without needing to insert new children.

        // Inserting as much of the word as possible without making new children.
        for i in 0..chars.len() {
            // If current node has the character as a child
            if curNode.children.contains_key(&chars[i]) {
                // Set current node to the child node.
                // Increment lastIndex
                curNode = curNode.children.get_mut(&chars[i]).unwrap();
                lastIndex = lastIndex + 1;
            } else {
                // If current node doesn't have children, set lastIndex to i and break the loop
                lastIndex = i;
                break;
            }
        }

        // If we inserted the whole word without needing to make new children, set the cur node to be the end of a word.
        if lastIndex == chars.len() {
            curNode.is_word = true;
        }
        // Otherwise, we need to make children for however many nodes are left.
        else {
            for i in lastIndex..chars.len() {
                // For the rest of the nodes, insert the new child.
                curNode.insert_below(chars[i], false);
                // Need to update curNode to the node just inserted.
                curNode = curNode.children.get_mut(&chars[i]).unwrap();
            }
        }

        // When the whole thing is inserted, set curNode to be the end of a word.
        curNode.is_word = true;
    }

    // Initialize a Trie from a vector of Strings.
    pub fn initTrie(wordList: &Vec<String>) -> Trie {
        let mut trie = Trie::makeTrie(); // Initialize an empty trie...

        for word in wordList.iter() {
            //...iterate through words and insert them in
            trie.insert(word);
        }
        // Return the trie afterwards
        return trie;
    }

    /*
    Finding a word and a prefix in a Trie have the same logic.
    Only difference is if the word is the end of a word (is_word = true)
    Therefore, the isWord parameter will be modified by reference, and changed to true if the prefix is a word.
    */
    pub fn findPrefix(&mut self, value: &str, isWord: &mut bool) -> bool {
        let chars: Vec<char> = value.chars().collect();
        let mut curNode = &mut self.rootNode;

        for i in 0..chars.len() {
            // If current node's children doesn't have it, then it cannot be in.
            if !curNode.children.contains_key(&chars[i]) {
                return false;
            } else {
                curNode = curNode.children.get_mut(&chars[i]).unwrap();
            }
        }
        // If we found the prefix (escaped the for-loop without returning false) and it's the end of a word, modify the isWord parameter.
        if curNode.is_word {
            *isWord = true;
        }
        return true;
    }
}

/* INITIALIZE CHARACTER AND COORDINATE ADJACENCY MAPS
PARAMETERS:
coordMap: Map of coordinate keys (y,x) and vectors with coords as values.
charMap: Map of characters of the board, and a HashSet of the characters adjacent to them.
board: Immutable array of string slices
*/
pub fn initMaps(
    coordMap: &mut HashMap<(usize, usize), Vec<(usize, usize)>>,
    charMap: &mut HashMap<char, HashSet<char>>,
    board: &[&str]) -> ()
{
    let len = board.len();

    // For each row in the board
    for (y, row) in board.iter().enumerate() {
        // For each char in the row
        for (x, ch) in row.chars().enumerate() {
            // If key (character) doesn't exist, insert a new HashSet as its value. Otherwise ignores this.
            charMap.entry(ch).or_insert(HashSet::new());
            // If coord doesnt exist, insert a new Vector into its values.
            coordMap.entry((y, x)).or_insert(Vec::new());

            for dr in [-1, 0, 1] {
                for dc in [-1, 0, 1] {
                    // If they're both 0, skip it. Don't include that coordinate.
                    if !(dr == 0 && dc == 0) {
                        // Need to support negative numbers, convert to i32
                        let newX = x as i32 + dr;
                        let newY = y as i32 + dc;

                        if inBounds(newX, newY, len) {
                            // If coords are inbounds. Insert the adjacent character into the Key's HashSet.
                            charMap.entry(ch).and_modify(|e| {
                                e.insert(board[newY as usize].chars().nth(newX as usize).unwrap());
                            });
                            // Also insert this adjacent coord into the coord's Vector
                            coordMap
                                .entry((y, x))
                                .and_modify(|v| v.push((newY as usize, newX as usize)));
                        }
                    }
                }
            }
        }
    }
}
// Helper function checks if a coordinate is in bounds of the board
pub fn inBounds(x: i32, y: i32, length: usize) -> bool {
    x >= 0 && x < (length as i32) && y >= 0 && y < (length as i32)
}

// FUNCTION TO PRUNE WORDS FROM THE WORD VECTOR
/*
Params: 
words, the wordList vector,  
charMap, the character adjacencies, 
boardSize, length * width of the board.
 */ 
pub fn pruneWords(
    wordVec: &Vec<String>,
    charMap: &HashMap<char, HashSet<char>>,
    boardSize: usize) -> Vec<String> {
    let mut newWords: Vec<String> = Vec::new();

    for word in wordVec {
        // If a word has # of chars <= # of tiles, and passes the test, push it to the newWords.
        if word.len() <=  boardSize && isPossible(word, &charMap) {
            newWords.push(word.to_string());
        }
    }
    return newWords;
}
// Helper function which checks if a word can be made from the Character Adjacency Map
pub fn isPossible(word: &str, charMap: &HashMap<char, HashSet<char>>) -> bool {
    // This shouldn't happen, but it's for avoiding unexpected panics, just in case.
    if word.len() == 0 {
        return false
    }
    // If word is length 1, just check if it has a key in charMap.
    if word.len() == 1 {
        return charMap.contains_key(&word.chars().nth(0).unwrap());
    }
    
    for i in 0..(word.len() - 1) {
        // For each character, and the next one after it...
        let curChar = word.chars().nth(i).unwrap();
        let nextChar = word.chars().nth(i + 1).unwrap();

        // If charMap doesn't contain a key for curChar, or nextChar isn't in that key's HashSet, return false.
        if !charMap.contains_key(&curChar) || !charMap.get(&curChar).unwrap().contains(&nextChar)
        {
            return false;
        }
    }
    // If the function gets here, the word can be made from the charMap.
    return true;
}

/*SEARCH ALGORITHM: Depth-First-Search
 PARAMS:
    board: The board. An immutable array of string slices.
    coordAdj: The coordinate adjacency HashMap.
    x,y: Coordinates in the board.
    trie: The Prefix Tree.
    curWord: Current word assembled by the board.
    prev: Vector of previously visited coordinates.
    foundWords: Set of words that have been found.
    found: The result. Not returned, just modified by reference.
 */
pub fn dfs(
    board: &[&str],
    coordAdj: &HashMap<(usize, usize), Vec<(usize, usize)>>,
    x: usize,
    y: usize,
    trie: &mut Trie,
    curWord: &mut String,
    prev: &mut Vec<(usize, usize)>,
    foundWords: &mut HashSet<String>,
    found: &mut HashMap<String, Vec<(usize, usize)>>
) -> () {
    let boardChar = board[y].chars().nth(x).unwrap();
    
    // "Visit" character/coordinate. Push to their respective variables
    curWord.push(boardChar);
    prev.push((y, x));
    
    let mut isWord = false;

    // If word is not a prefix, unvisit char and coord, and return from the function.
    if !(trie.findPrefix(&curWord, &mut isWord)) {
        curWord.pop();
        prev.pop();
        return ()
    }

    // If word was found, and it hasn't been found yet put it into foundWords and found 
    if isWord && !foundWords.contains(curWord){
        foundWords.insert(curWord.to_string());
        found.insert(curWord.to_string(), (&prev).to_vec());
    }

    let coord = (y, x);

    // For each child coordinate of the coord key,
    for child in coordAdj.get(&coord).unwrap().iter(){
        // if it's has not been visited already, recursively rerun the function with the new coordinate.
        if !prev.contains(&child) {
            let newY = child.0;
            let newX = child.1;

            dfs(board, coordAdj, newX, newY, trie, curWord, prev, foundWords, found);
        }
    }
    // "unvisit" the character/coordinate when all is said and done.
        // Only if they're not empty though, since in the initial call, they are empty.
    if curWord.len()!=0 {
        curWord.pop();
    }
    if prev.len()!=0 {
        prev.pop();
    }

}


// MAIN ALGORITHM
fn boggle(board: & [&str], words: & Vec<String>) -> HashMap<String, Vec<(usize, usize)>>
{
    // COORD ADJACENCIES & CHAR ADJACENCIES
    // Keys: Coordinates (y,x).   Values: Vector of Coordinates (y, x)
    let mut coordAdj: HashMap<(usize, usize), Vec<(usize, usize)>> = HashMap::new();
    // Keys: Characters.   Values: HashSets of characters
    let mut charAdj: HashMap<char, HashSet<char>> = HashMap::new();
    initMaps(&mut coordAdj, &mut charAdj, &board);

    // PRUNE WORDS
    let newWords = pruneWords(&words, &charAdj, board.len() *  board.len());

    // INITIALIZE TRIE FROM PRUNED WORDS
    let mut trie: Trie = Trie::initTrie(&newWords);

    let mut found: HashMap<String, Vec<(usize, usize)>> = HashMap::new();

    // For each coordinate on the board
    for y in 0..board.len()
    {
        for x in 0..board.len()
        {
            // Do a Depth-First Search from that coordinate.
            dfs(&board, &coordAdj, x, y, &mut trie, &mut String::from(""), &mut Vec::new(), &mut HashSet::new(), &mut found) ;
        }
    }

    return found
}
    
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

