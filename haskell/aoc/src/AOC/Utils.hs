{-# LANGUAGE OverloadedStrings #-}

module AOC.Utils
  ( readItemsFromFile,
    readItemsFromFileWith,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readItemsFromFile :: (Read a) => FilePath -> IO [a]
readItemsFromFile p = init . T.splitOn "\n" <$> TIO.readFile p >>= return . fmap (read . T.unpack)

readItemsFromFileWith :: (Read a) => FilePath -> (T.Text -> a) -> IO [a]
readItemsFromFileWith p f = init . T.splitOn "\n" <$> TIO.readFile p >>= return . fmap f
