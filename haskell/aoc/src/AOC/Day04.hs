{-# LANGUAGE TupleSections #-}

module AOC.Day04 where

import AOC.Utils (readItemsFromFileWith)
import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (isDigit)
import Data.Functor (($>), (<&>))
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP as P

data Height = HeightCm Integer | HeightIn Integer
  deriving (Eq, Show)

data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other
  deriving (Eq, Show)

data Entry = Entry
  { entryBirthYear :: Integer,
    entryIssueYear :: Integer,
    entryExpYear :: Integer,
    entryHeight :: Height,
    entryHairColor :: T.Text,
    entryEyeColor :: EyeColor,
    entryPID :: T.Text,
    cid :: T.Text
  }
  deriving (Eq, Show)

parse :: ReadP a -> String -> Maybe a
parse p str =
  case x of
    [] -> Nothing
    [(x, _)] -> Just x
    _ -> Nothing
  where
    x = readP_to_S p str

number :: ReadP Integer
number = read <$> munch1 isDigit

numInRange :: Integer -> Integer -> ReadP Integer
numInRange min max = do
  n <- number
  when (n < min || n > max) pfail
  return n

birthYear :: ReadP Integer
birthYear = numInRange 1920 2002

issueYear :: ReadP Integer
issueYear = numInRange 2010 2020

expYear :: ReadP Integer
expYear = numInRange 2020 2030

height :: ReadP Height
height = do
  num <- number
  cm_or_inch <- string "cm" <|> string "in"
  case cm_or_inch of
    "cm" -> if num >= 150 && num <= 193 then return $ HeightCm num else pfail
    "in" -> if num >= 59 && num <= 76 then return $ HeightIn num else pfail
    _ -> error "wat"

hairColor :: ReadP T.Text
hairColor = P.char '#' *> P.count 6 hex <* P.eof <&> T.pack
  where
    hex = satisfy (\x -> x `elem` ['0' .. '9'] || x `elem` ['a' .. 'f'])

eyeColor :: ReadP EyeColor
eyeColor =
  (string "amb" $> Amber)
    <|> (string "blu" $> Blue)
    <|> (string "brn" $> Brown)
    <|> (string "gry" $> Gray)
    <|> (string "grn" $> Green)
    <|> (string "hzl" $> Hazel)
    <|> (string "oth" $> Other)

pid :: ReadP T.Text
pid = P.count 9 num <* P.eof <&> T.pack
  where
    num = satisfy (\x -> x `elem` ['0' .. '9'])

p :: T.Text -> Maybe [(T.Text, T.Text)]
p t = undefined
  where
    toTuple xs = (head xs, xs !! 1)
    tagMap = toTuple . T.splitOn ":" <$> T.splitOn " " t
    mandatoryTags = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    mandatory = sequence $ lookup <$> mandatoryTags <*> [tagMap]
    optionalTags = ["cid"]
    optional = sequence $ lookup <$> optionalTags <*> [tagMap]

parseEntry :: T.Text -> Maybe [(T.Text, T.Text)]
parseEntry t = case sequence (lookupTags <*> [bag]) <&> zip tags of
  (Just entries) -> Just entries <> opts
  Nothing -> Nothing
  where
    bag = toTuple . T.splitOn ":" <$> T.splitOn " " t
    tags = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    lookupTags = lookup <$> tags
    toTuple xs = (head xs, xs !! 1)
    opts = sequence [("cid" :: T.Text,) <$> lookup "cid" bag]

validateEntry :: [(T.Text, T.Text)] -> Maybe Entry
validateEntry bag =
  Entry <$> byr
    <*> iyr
    <*> eyr
    <*> hgt
    <*> hcl
    <*> ecl
    <*> pID
    <*> Just "cid"
  where
    byr = lookup "byr" bag >>= parse birthYear . T.unpack
    iyr = lookup "iyr" bag >>= parse issueYear . T.unpack
    eyr = lookup "eyr" bag >>= parse expYear . T.unpack
    hgt = lookup "hgt" bag >>= parse height . T.unpack
    hcl = lookup "hcl" bag >>= parse hairColor . T.unpack
    ecl = lookup "ecl" bag >>= parse eyeColor . T.unpack
    pID = lookup "pid" bag >>= parse pid . T.unpack

getInputData :: FilePath -> IO [T.Text]
getInputData p = do
  input <- readItemsFromFileWith p id
  return $ combine <$> split input
  where
    split = S.splitWhen (== "")
    combine = mconcat . L.intersperse " "

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
  input <- getInputData p
  putStrLn $ "got " ++ show (length input) ++ " entries"
  let entries = catMaybes $ parseEntry <$> input
  putStrLn $ "found " ++ show (length entries) ++ " valid entries"

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
  input <- getInputData p
  putStrLn $ "got " ++ show (length input) ++ " entries"
  let entries = catMaybes $ validate <$> input
  putStrLn $ "found " ++ show (length entries) ++ " valid entries"
  where
    validate :: T.Text -> Maybe Entry
    validate s = parseEntry s >>= validateEntry

tests :: [T.Text]
tests =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929",
    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm",
    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"
  ]
