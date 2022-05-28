{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char
import Data.Foldable
import Data.List (nub, sort)
import Debug.Trace

type Letter = Char

type Score = Int

type Position = Int

type Wrd = String

data LetterScore = LetterScore
  { score :: Score,
    letter :: Letter
  }
  deriving (Show, Eq, Ord)

data LetterPositionScore = LetterPositionScore
  { sc :: Score,
    ltr :: Letter,
    pos :: Position
  }
  deriving (Show, Eq, Ord)

data WordScore = WordScore Score Wrd deriving (Eq, Ord)

instance Show WordScore where
  show (WordScore s w) = " " ++ w ++ " (" ++ show s ++ ")"

scoreWords :: [LetterScore] -> [LetterPositionScore] -> [Wrd] -> [WordScore]
scoreWords ls lps = map (scoreWord ls lps)

scoreWord :: [LetterScore] -> [LetterPositionScore] -> Wrd -> WordScore
scoreWord letterScores letterPositionScores word =
  WordScore
    ( singleLetterScore
        + letterPositionScore
    )
    word
  where
    singleLetterScore :: Score = foldl (\x l -> x + getLetterScore l letterScores) 0 (nub word)
    letterPositionScore :: Score =
      foldl
        ( \x (index, l) ->
            getLetterPositionScore l index letterPositionScores
        )
        0
        (zip [0 .. length word - 1] word)

getLetterScore :: Letter -> [LetterScore] -> Int
getLetterScore ltr scores = maybe 0 score (find (\l -> letter l == ltr) scores)

getLetterPositionScore :: Letter -> Position -> [LetterPositionScore] -> Score
getLetterPositionScore l p scores = maybe 0 sc $ find (\ll -> ltr ll == l && pos ll == p) scores

scoreLetter :: Letter -> [Wrd] -> LetterScore
scoreLetter l words = LetterScore {letter = l, score = length (filter (\word -> l `elem` word) words)}

scoreLetters :: [Wrd] -> [LetterScore]
scoreLetters words = [scoreLetter l words | l <- ['a' .. 'z']]

scoreLetterPosition :: Letter -> Position -> [Wrd] -> LetterPositionScore
scoreLetterPosition l p words = LetterPositionScore {ltr = l, pos = p, sc = length (filter (\word -> word !! p == l) words)}

scoreLetterPositions :: [String] -> [LetterPositionScore]
scoreLetterPositions words = [scoreLetterPosition l p words | l <- ['a' .. 'z'], p <- [0 .. 4]]

extractWord :: WordScore -> Wrd
extractWord (WordScore _ w) = w

generateGuess :: [Wrd] -> Wrd
generateGuess words = trace ("I am considering: " ++ (show $ take 5 $ reverse $ sort wordScores)) $
  extractWord $ maximum wordScores
  where
    wordScores = scoreWords (scoreLetters words) (scoreLetterPositions words) words

processLastWord :: [Char] -> [Char] -> [Wrd] -> [Wrd]
processLastWord lstGuess lstGuessResult wordList =
  foldr (\x wrds -> processLetterResult (snd x) (fst x) correctLetters wrds) wordList indexedIntermediateResult
  where
    intermediateResult = (zip lstGuess lstGuessResult)
    indexedIntermediateResult = zip [0 .. length lstGuess] intermediateResult
    correctLetters = map (\x -> fst x) $ filter (\x -> snd x == 'G' || snd x == 'Y') intermediateResult

processLetterResult :: (Char, Char) -> Position -> [Char] -> [Wrd] -> [Wrd]
processLetterResult (l, 'G') p _ words = filter (\wrd -> wrd !! p == l) words
processLetterResult (l, 'Y') p _ words = filter (\wrd -> wrd !! p /= l && l `elem` wrd) words
processLetterResult (l, 'X') p correct words = filter (\wrd -> countOf l wrd <= countOf l correct) words
processLetterResult (l, _) _ _ words = words

countOf :: Char -> [Char] -> Int
countOf x = length . filter (==x)

getLastResult :: IO String
getLastResult =
  do
    putStrLn "What was the result?"
    putStrLn "Enter 'G' for each correctly placed letter, 'Y' for each letter in the word but in the wrong place, and 'X' for incorrect letters"
    getResultInput

getResultInput :: IO String
getResultInput = do
  result <- getLine
  if all (\x -> x `elem` ['G', 'Y', 'X']) (map toUpper result) && length result == 5
    then return (map toUpper result)
    else do
      putStrLn "Invalid input. Try again"
      getResultInput

playRound :: Int -> [Wrd] -> [Char] -> [Char] -> IO ()
playRound 0 wordList lstGuess lstResult = do
  putStrLn "Better luck next time!"
  return ()
playRound rnd wordList lstGuess lstResult = do
  let remainingWords = processLastWord lstGuess lstResult wordList
  let guess = generateGuess remainingWords
  putStrLn $ "I guess: " ++ guess
  result <- getLastResult
  if all ((== 'G') . toUpper) result
    then do
      putStrLn "Hurray!"
      putStrLn $ "I guessed the word in " ++ show (7 - rnd)
      return ()
    else playRound (rnd - 1) remainingWords guess result
  return ()

main :: IO ()
main = do
  wordFile <- readFile "answers.txt"
  let words :: [String] = lines wordFile
  playRound 6 words [] []
