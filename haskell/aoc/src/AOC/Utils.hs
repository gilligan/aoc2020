{-# LANGUAGE OverloadedStrings #-}

module AOC.Utils
  ( readItemsFromFile,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readItemsFromFile :: (Read a) => FilePath -> IO [a]
readItemsFromFile p = init . T.splitOn "\n" <$> TIO.readFile p >>= return . fmap (read . T.unpack)
