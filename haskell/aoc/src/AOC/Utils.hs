{-# LANGUAGE OverloadedStrings #-}

module AOC.Utils
  ( -- file functions
    readItemsFromFile,
    readItemsFromFileWith,
    -- parsing functions
    word,
    number,
    signedNum,
    parseMaybe
  )
where

import Data.Char (isAlpha, isDigit)
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.ParserCombinators.ReadP as P
import Control.Applicative ((<|>))

readItemsFromFile :: (Read a) => FilePath -> IO [a]
readItemsFromFile p = (init . T.splitOn "\n" <$> TIO.readFile p) <&> fmap (read . T.unpack)

readItemsFromFileWith :: FilePath -> (T.Text -> a) -> IO [a]
readItemsFromFileWith p f = (init . T.splitOn "\n" <$> TIO.readFile p) Data.Functor.<&> fmap f

word :: ReadP String
word = munch1 isAlpha

number :: ReadP Integer
number = read <$> munch1 isDigit

signedNum :: ReadP Integer
signedNum = negNum <|> posNum
    where
        posNum = char '+' *> munch1 isDigit <&> read
        negNum = char '-' *> munch1 isDigit <&> ((* (- 1)) . read)


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe p str =
  case x of
    [] -> Nothing
    ((x,_):_) -> Just x
  where
    x = readP_to_S p str
