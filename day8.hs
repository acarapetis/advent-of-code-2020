import AOC
import Asm
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (fromJust)

hasLoop :: ProgramState -> Bool
hasLoop (ProgramState {history=xs}) = length xs > (S.size $ S.fromList xs)

doneWithProgram ps = hasLoop ps || finished ps
lastUniqueState :: Instructions -> ProgramState
lastUniqueState = last . takeWhile (not.doneWithProgram) . runProgram

part1 = show . acc . memory . lastUniqueState . readProgram

tweakInstruction (Nop n) = Jmp n
tweakInstruction (Jmp n) = Nop n
tweakInstruction x = x

tweakProgram :: Instructions -> Int -> Instructions
tweakProgram p n = p0 ++ (tweakInstruction x):p1
    where (p0, x:p1) = splitAt n p

tweakedPrograms p = [tweakProgram p n | n <- [0..(length p) - 1]]


part2 = show . acc . memory . fromJust .
    find (not.hasLoop) . map (tick . lastUniqueState) . tweakedPrograms . readProgram

main = do
    input <- readFile "day8.txt"
    putStrLn $ part1 input
    putStrLn $ part2 input
