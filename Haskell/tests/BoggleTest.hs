module Main where
    import Test.HUnit
    import Data.List
    import Data.Array.IO
    import System.Random
    import Control.Monad
    import Data.Char (isSpace)
    import Data.Time.Clock.POSIX (getPOSIXTime)
    import Data.Time
    import qualified Boggle
    import qualified Data.Set as Set
    import qualified System.Exit as Exit

    main = do
         status <- runTestTT all_tests
         if failures status > 0 || errors status > 0 then Exit.exitFailure else Exit.exitSuccess

    -- run tests with:
    --     cabal test

    boggle = Boggle.boggle
       
    all_tests = TestList $ test_1_mass_4x4 ++ 
                           test_2a_given_2x2 ++ test_2b_given_4x4 ++
                           test_3a_small_2x2 ++ test_3b_medium_4x4 ++ test_3c_large_8x8 ++ test_3d_mega_16x16

    n_rand_tests 0 _ = return True
    n_rand_tests n words = do
        d1  <- shuffle "rifobx"
        d2  <- shuffle "ifehey"
        d3  <- shuffle "denows"
        d4  <- shuffle "utoknd"
        d5  <- shuffle "hmsrao"
        d6  <- shuffle "lupets"
        d7  <- shuffle "acitoa"
        d8  <- shuffle "ylgkue"
        d9  <- shuffle "aaaaaa"
        d10 <- shuffle "ehispn"
        d11 <- shuffle "vetign"
        d12 <- shuffle "baliyt"
        d13 <- shuffle "ezavnd"
        d14 <- shuffle "ralesc"
        d15 <- shuffle "uwilrg"
        d16 <- shuffle "pacemd"
        bs <- shuffle [ d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16]    
        let board = [ [head (bs !! 0), head (bs !! 1), head (bs !! 2), head (bs !! 3)], 
                      [head (bs !! 4), head (bs !! 5), head (bs !! 6), head (bs !! 7)], 
                      [head (bs !! 8), head (bs !! 9), head (bs !! 10), head (bs !! 11)], 
                      [head (bs !! 12), head (bs !! 13), head (bs !! 14), head (bs !! 15)] ]   
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "1: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "1: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "1: Word coords not legal" a3 True

        n_rand_tests (n-1) words

    test_1_mass_4x4 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_500.txt"
        let raw = lines contents
        let words = map trim raw
        res <- n_rand_tests 1000 words
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "1_score.txt" $ "1: Passed in " ++ (show t2) ++ " ms"
        assertEqual "" True True ]

    test_2a_given_2x2 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_3000.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "ea", "st" ]

        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "2a: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "2a: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "2a: Word coords not legal" a3 True
        score <- return $ getScore found
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "2a_score.txt" $ "2a: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms"
        assertEqual "" True True
        ]

    test_2b_given_4x4 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_3000.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "isuo", "osve", "nepa", "ntsu" ]
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "2b: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "2b: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "2b: Word coords not legal" a3 True
        score <- return $ getScore found

        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "2b_score.txt" ("2b: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms")
        assertEqual "" True True
        ]

    test_3a_small_2x2 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_scrabble_2019.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "ea", "st" ]
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "3a: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "3a: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "3a: Word coords not legal" a3 True
        score <- return $ getScore found

        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "3a_score.txt" ("3a: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms")
        assertEqual "" True True
        ]

    test_3b_medium_4x4 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_scrabble_2019.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "isuo", "osve", "nepa", "ntsu" ]
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "3b: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "3b: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "3b: Word coords not legal" a3 True
        score <- return $ getScore found

        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "3b_score.txt" ("3b: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms")
        assertEqual "" True True
        ]

    test_3c_large_8x8 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_scrabble_2019.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "ocneasra", "crishtir", "llannren", "genssaqn",
                      "damcobnu", "nroosyen", "atsarson", "bessnnis" ]
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "3c: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "3c: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "3c: Word coords not legal" a3 True
        score <- return $ getScore found

        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "3c_score.txt" ("3c: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms")
        assertEqual "" True True
        ]

    test_3d_mega_16x16 = 
        [ TestCase $ do
        start <- getCurrentTime
        contents <- readFile "lists/word_list_scrabble_2019.txt"
        let raw = lines contents
        let words = map trim raw
        let board = [ "aqoausieartuelro", "lnucrsursdirztom", "qcaclodqtyiycrav", "desmpantsemtdest", 
                      "itqeetroabnoahna", "dnecrpolvnezsmim", "plorssisttugctog", "balvridnmolsbanv",
                      "ojnaoyloifgaesza", "nmellsennpirmcin", "lsalnmucrlarmbam", "pemhzarnyeclpesr",
                      "isnoustaosceicio", "rdeasdoldlesrlom", "pgurlvoclserbmem", "siasnaprmurhtosc" ]
        
        let found = boggle board words
        let a1 = wordsLegal found words
        assertEqual "3d: Word not legal" a1 True
        let a2 = wordsInBoard found board
        assertEqual "3d: Word not in board" a2 True
        let a3 = wordCoordsOK found board
        assertEqual "3d: Word coords not legal" a3 True
        score <- return $ getScore found

        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        writeFile "3d_score.txt" ("3d: Score = " ++ (show score) ++ " in " ++ (show t2) ++ " ms")
        assertEqual "" True True
        ]

    wordsLegal found words = 
        let word_set = Set.fromList words
            found_set = Set.fromList $ map (\(word, _) -> word) found
            words_legal = map (\(word, _) -> Set.member word word_set) found
        in  if (Set.size found_set) == (length words_legal) then foldl (&&) True words_legal else False

    wordsInBoard :: [ (String, [ (Int, Int) ] ) ] -> [[Char]] -> Bool
    wordsInBoard found board = 
        let res = map (\entry -> validateWord board entry) found 
        in  foldl (&&) True res

    wordCoordsOK found board =
        let res = map (\entry -> validateCoords entry) found 
        in  foldl (&&) True res

    validateWord :: [[Char]] -> ( [Char], [ (Int, Int) ] ) -> Bool
    validateWord board (word, coords) = 
        let zipped = zip word coords 
            res = map (\(ch, (x, y)) -> ch == (getCell board x y)) zipped
        in  foldl (&&) True res
        
    validateCoords (_, coords) = 
        let dup = length coords == Set.size (Set.fromList coords)
            xc = [ x | (x, _) <- coords ]
            x_ok = foldl (&&) True $ map (\(x1, x2) -> abs (x1-x2) <= 1) $ (zip xc $ tail xc) 
            yc = [ y | (_, y) <- coords ]
            y_ok = foldl (&&) True $ map (\(y1, y2) -> abs (y1-y2) <= 1) $ (zip yc $ tail yc) 
        in  dup && x_ok && y_ok

    getCell :: [[Char]] -> Int -> Int -> Char
    getCell board x y = (board !! x) !! y

    getScore found = 
        let w_scores = [1, 2, 4, 6, 9, 12, 16, 20]  
            scores = map (\(w, _) -> if length w <= 8 then w_scores !! ((length w) - 1) else 20) found
        in  foldl (+) 0 scores
    
    shuffle :: [a] -> IO [a]
    shuffle [] = return []
    shuffle xs = do
        i <- getStdRandom $ randomR (0, length xs - 1)
        let (left, (a:right)) = splitAt i xs
        fmap (a:) (shuffle $ left ++ right)
    
    trim :: String -> String
    trim = f . f
        where f = reverse . dropWhile isSpace