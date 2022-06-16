module Main where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust, fromMaybe) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout) -- [6]
import System.Random (randomRIO) -- [7]

{-
TODO
- guesses left should NOT be counted against for valid guesses
-}
minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9
maxGuesses:: Int
maxGuesses = 7


-- WORDS LIST
newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = let l = length (w :: String) in l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- PUZZLE - guess, filled in so far, guessed
data Puzzle = Puzzle String [Maybe Char] [Char]


instance Show Puzzle where
  show (Puzzle wordToGuess discovered guessed) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ "\nGuessed so far: " ++ guessed
    ++ "\nGuesses left: " ++ show (guessesLeft wordToGuess guessed)


freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ alreadyGuessed) c = c `elem` alreadyGuessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s) where
  zipper guessed wordChar guessChar =
    if wordChar == guessed then Just wordChar
    else guessChar
  newFilledInSoFar = zipWith (zipper c) word filledInSoFar


-- HANDLE GUESS
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- TODO account for correct guesses
guessesLeft :: String -> [Char] -> Int
guessesLeft wordToGuess guessed = do
  -- TODO
  -- use map or something to traverse the word
  -- for each guess already in the word, subtract
  -- one from the grouped term below
  return maxGuesses - (length guessed)



-- GAME OVER
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (guessesLeft wordToGuess guessed) < 0 then
    do
      putStrLn $ "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else return ()


-- GAME WIN
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do
      putStrLn $ "You win!"
      exitSuccess
  else return ()


-- RUN GAME
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"



-- MAIN
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
