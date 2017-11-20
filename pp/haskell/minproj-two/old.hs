
 import System.Random
 import System.IO

-- Finding a random word in a file --

someIndex :: IO Int

someIndex = getStdRandom (randomR (1,6))

selectWord = do
                myhandle <- openFile "words.txt" ReadMode
                words <- hGetContents myhandle
                index <- someIndex
                let someWord = (lines words) !! index in return someWord

-- Displaying what is known about the word

hyphens word = map (\z -> '-') word

diff guess word = zipWith (\c1 c2-> if c1==c2 then c1 else '-') guess word


-- The read-eval-print loop of the game

hangman 0 word = do
                   putStrLn ("Sorry, but the correct word was " ++ word)
                   return ()
hangman n word = do
              guess <- getLine
              if guess == word then
                                   do
                                     putStrLn "Hurra!"
                                     return ()
                                else
                                    do
                                      putStrLn (diff guess word)
                                      putStrLn $ "You have " ++ (show (n-1)) ++ " attempts left"
                                      hangman (n-1) word
                                      return ()


-- The main function

main = do
            word <- selectWord
            putStrLn $ "The word contains " ++ show (length(word)) ++ " letters"
            let lgd = length word in putStrLn (hyphens word)
            hangman (length(word)) word
            return ()