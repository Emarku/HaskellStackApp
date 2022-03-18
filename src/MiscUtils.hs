{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MiscUtils
  ( -- * Utilities

    --

    -- | This module contains common and useful functions
    -- that will be shared by all other modules
    charReplace,
    removeUntil,
    removeUntilChar,
    getUntilChar,
    trimList,
    stripStr,
    trim,
    seperator,
    splitAtRecur,
    showFP,
    putStrLnListWithNumber,
    listStringToInt,
  )
where

import Control.Monad (mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, replace, strip, unpack)
import Numeric (showFFloat)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Replacing a character with another one in the String
charReplace :: Eq b => b -> b -> [b] -> [b]
charReplace a b = map $ fromMaybe b . mfilter (/= a) . Just

-- | Trim the space on the left
trimLeft :: String -> String
trimLeft = dropWhile isSpace

-- | Trim the space on the left
trimRight :: String -> String
trimRight = dropWhileEnd isSpace

-- | The trim function, to remove all extra white space in String
trim :: String -> String
trim = trimLeft . trimRight

-- | A map function for String list to use trim
trimList :: [String] -> [String]
trimList = map trim

-- | Drop the String in the list
-- Unitl it reaches the string that contains the desired substring
removeUntil :: String -> [String] -> [String]
removeUntil str [] = []
removeUntil str (x : xs) = if BSU.fromString str `BS.isInfixOf` BSU.fromString x then x : xs else removeUntil str xs

-- | Drop character in String
-- Until it reaches the desired character and
-- returning the remaining parts
removeUntilChar :: Char -> String -> String
removeUntilChar c [] = []
removeUntilChar c (x : xs) = if c == x then xs else removeUntilChar c xs

-- | Get characters in String
-- Until it reaches the desired character and
-- returning the result string
getUntilChar :: Char -> String -> String
getUntilChar c [] = []
getUntilChar c (x : xs) = if c /= x then x : getUntilChar c xs else []

-- | Filter out all characters that is in the first string
-- with the second string
stripStr :: String -> String -> String
stripStr = filter . flip notElem

-- | Return a seperator with the desired character and length
seperator :: Char -> Int -> String
seperator c 0 = ""
seperator c i = c : seperator c (i -1)

-- | Split a long string at every n characters
splitAtRecur :: Int -> String -> [String]
splitAtRecur n [] = []
splitAtRecur n str = do
  let a = splitAt n str
  fst a : splitAtRecur n (snd a)

listStringToInt :: [String] -> [Maybe Int]
listStringToInt = map (\x -> readMaybe x :: Maybe Int)

-- | Show the float number properly with all digits and 2dp
showFP :: Float -> String
showFP x = showFFloat Nothing x ""

-- | putStrLn with a list and number with prefix
putStrLnListWithNumber_ :: Int -> [String] -> IO ()
putStrLnListWithNumber_ n [] = return ()
putStrLnListWithNumber_ n (l : ls) = do
  putStrLn $ show n ++ ".\t" ++ l
  putStrLnListWithNumber_ (n + 1) ls

-- | putStrLnListWithNumber_ with the starting number = 1
putStrLnListWithNumber :: [String] -> IO ()
putStrLnListWithNumber = putStrLnListWithNumber_ 1