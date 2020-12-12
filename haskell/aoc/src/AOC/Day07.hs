{-# LANGUAGE TupleSections #-}

module AOC.Day07 where

import AOC.Utils (number, parseMaybe, readItemsFromFileWith, word)
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Map ((\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum, getSum)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP as P
  ( ReadP,
    count,
    satisfy,
    sepBy1,
    string,
  )

type Bag = (String, [(Integer, String)])

type BagMap = M.Map String [(Integer, String)]

bags :: ReadP String
bags = count 2 (word <* satisfy isSpace) <* (string "bag" <|> string "bags") <&> unwords

emptyBag :: ReadP Bag
emptyBag = bags <* string " contain no other bags." <&> (,[])

nonEmptyBag :: ReadP Bag
nonEmptyBag = do
  b <- bags <* string " contain "
  bs <- sepBy1 ((,) <$> (number <* string " ") <*> bags) (string ", ") <* string "."
  return (b, bs)

bagEntry :: ReadP Bag
bagEntry = emptyBag <|> nonEmptyBag

containsBag :: String -> [(Integer, String)] -> Bool
containsBag s cs = s `elem` concatMap ((: []) . snd) cs

findBagParents :: BagMap -> String -> [String]
findBagParents bagMap s
  | M.null bagMap = []
  | otherwise = bsKeys ++ L.nub (concatMap (findBagParents (bagMap \\ bs)) (M.keys bs))
  where
    bagsContaining s = M.filter (containsBag s)
    bs = bagsContaining s bagMap
    bsKeys = M.keys bs

getBagsCount :: BagMap -> String -> Sum Integer
getBagsCount bagMap s
  | M.null bagMap = 1
  | otherwise = 1 + foldMap go children
  where
    go :: (Integer, String) -> Sum Integer
    go (cnt, b) = (cnt *) <$> getBagsCount bagMap b
    children = fromMaybe [] (M.lookup s bagMap)

solve :: Show a => FilePath -> (BagMap -> a) -> IO ()
solve p solver = do
  input <- readItemsFromFileWith p T.unpack
  case traverse (parseMaybe bagEntry) input of
    Just bags -> print $ solver (M.fromList bags)
    Nothing -> print "oops, parsing failed"

solvePart1 :: FilePath -> IO ()
solvePart1 p = solve p (\bs -> length $ findBagParents bs "shiny gold")

solvePart2 :: FilePath -> IO ()
solvePart2 p = solve p (\bs -> getSum $ getBagsCount bs "shiny gold" - 1)
