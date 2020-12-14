module AOC.Day08 where

import AOC.Utils (parseMaybe, readItemsFromFileWith, signedNum)
import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP as P
  ( ReadP,
    string,
  )

data Instruction n
  = Nop n
  | Acc n
  | Jmp n
  deriving (Show)

data Ctx = Ctx
  { acc :: Integer, -- current accumulator value
    pos :: Integer, -- current position
    program :: [Inst], -- program being executed
    history :: [Integer] -- history of previously executed instructions
  }

instance Show Ctx where
  show (Ctx acc pos _ _) = "accu: " ++ show acc ++ " " ++ "pos: " ++ show pos

data State a = Running a | Stopped a
  deriving (Show, Eq)

type ProgState = State Ctx

mkState :: [Inst] -> ProgState
mkState is = Running $ Ctx 0 0 is []

type Inst = Instruction Integer

exec :: (Inst, ProgState) -> ProgState
exec (_, st@(Stopped _)) = st
exec (Acc n, Running c) = Running $ c {acc = acc c + n, pos = pos c + 1, history = history c ++ [pos c]}
exec (Nop _, Running c) = Running $ c {pos = pos c + 1, history = history c ++ [pos c]}
exec (Jmp n, Running c) = Running $ c {pos = pos c + n, history = history c ++ [pos c]}

preExec :: ProgState -> Either ProgState (Inst, ProgState)
preExec s@(Stopped _) = Left s
preExec s@(Running ctx)
  | pos ctx `elem` history ctx || emptyProgram || illegalPos = Left (Stopped ctx)
  | otherwise = Right (inst, s)
  where
    emptyProgram = null $ program ctx
    illegalPos = pos ctx >= (toInteger . length $ program ctx)
    inst = program ctx !! fromInteger (pos ctx)

runPogram :: ProgState -> ProgState
runPogram s@(Stopped _) = s
runPogram s@(Running _) = runPogram $ either id exec (preExec s)

inst :: ReadP Inst
inst =
  (string "acc " *> signedNum <&> Acc)
    <|> (string "jmp " *> signedNum <&> Jmp)
    <|> (string "nop " *> signedNum <&> Nop)

getInput :: FilePath -> IO [String]
getInput p = readItemsFromFileWith p T.unpack

patchProg :: [Inst] -> (Inst -> Inst) -> Int -> [Inst]
patchProg prog f index = replace index (f $ prog !! index) prog
  where
    replace :: (Num a, Ord a) => a -> b -> [b] -> [b]
    replace _ _ [] = []
    replace 0 a (_ : xs) = a : xs
    replace n a (x : xs) =
      if n < 0
        then x : xs
        else x : replace (n -1) a xs

findProgram :: [[Inst]] -> (ProgState -> Bool) -> Maybe ProgState
findProgram [] _ = Nothing
findProgram (x : xs) f = if f progResult then Just progResult else findProgram xs f
  where
    progResult = runPogram $ mkState x

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
  input <- getInput p
  let program = fromJust $ traverse (parseMaybe inst) input
  let noppedJumpsPrograms = patchProg program jmpToNop <$> L.findIndices isJmp program
  let jumpedNopsPrograms = patchProg program nopToJmp <$> L.findIndices isNop program
  print $ findProgram (noppedJumpsPrograms ++ jumpedNopsPrograms) isValidProgram
  where
    jmpToNop (Jmp x) = Nop x
    jmpToNop x = x

    nopToJmp (Nop x) = Jmp x
    nopToJmp x = x

    isJmp (Jmp _) = True
    isJmp _ = False

    isNop (Nop _) = True
    isNop _ = False

    isValidProgram (Running _) = False
    isValidProgram (Stopped ctx)
      | pos ctx == (toInteger . length $ program ctx) = True
      | otherwise = False

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
  input <- getInput p
  case traverse (parseMaybe inst) input of
    Just is -> print $ runPogram (mkState is)
    Nothing -> print "oops, failed to parse the input!"
