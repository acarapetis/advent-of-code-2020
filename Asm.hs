module Asm (module Asm) where

import AOC
import Control.Monad.Writer
import qualified Data.Set as S

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Ord, Show, Eq)
type Instructions = [Instruction]
type LOC = (Int, Instruction)

data Memory = Memory { acc :: Int } deriving (Show, Eq)
type InstructionResult = (Memory, Int)

perform :: Memory -> Instruction -> InstructionResult
perform (Memory n) x = (Memory (apply n x), toJump n x) where
    apply n (Acc x) = n + x
    apply n _ = n
    toJump _ (Jmp x) = x
    toJump _ _ = 1

readInstruction :: String -> Int -> Instruction
readInstruction "nop" n = Nop n
readInstruction "jmp" n = Jmp n
readInstruction "acc" n = Acc n

instructionP :: Parser Instruction
instructionP = do
    cmd <- many1 letter
    spaces
    sign <- ("" <$ string "+") <|> (string "-")
    num <- many1 digit
    return $ readInstruction cmd (read (sign++num) :: Int)

programP = many1 (instructionP <* optional endOfLine)
readProgram = parseOrDie programP

data ProgramState = ProgramState {
    memory :: Memory,
    program :: Instructions,
    line :: Int,
    history :: [LOC]
} deriving (Show)

startProgram :: Instructions -> ProgramState
startProgram program = ProgramState (Memory 0) program 0 []

tick :: ProgramState -> ProgramState
tick (ProgramState s p n xs) = ProgramState s1 p (n + jmp) ((n,x):xs) where
    x = p !! n
    (s1, jmp) = perform s x

runProgram :: Instructions -> [ProgramState]
runProgram = iterate tick . startProgram

finished p = (line p) >= (length $ program p)
