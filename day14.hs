import AOC
import Data.Bits
import Data.Map (Map, insert, elems, empty)
import Data.Int (Int64)

data Tweak = Noop | Fix0 | Fix1 deriving (Eq)
instance Show Tweak where
    show Noop = "X"
    show Fix0 = "0"
    show Fix1 = "1"
type Mask = [Tweak]

emptyMask = take 36 $ repeat Noop

apply :: Mask -> Int64 -> Int64
apply mask num = foldl setOrClear num ops where
    ops = zip [35,34..0] mask
    setOrClear n (_,Noop) = n
    setOrClear n (i,Fix0) = clearBit n i
    setOrClear n (i,Fix1) = setBit n i

data MState = MState Mask (Map Int64 Int64) deriving Show
emptyState = MState emptyMask empty
data Instruction = SetMask Mask | SetMem Int64 Int64 deriving Show

tweakP :: Parser Tweak
tweakP = choice
    [ Noop <$ char 'X'
    , Fix0 <$ char '0'
    , Fix1 <$ char '1'
    ]

maskP = many1 tweakP
setMaskP = do
    try $ string "mask = "
    mask <- maskP
    endOfLine
    return $ SetMask mask

setMemP :: Parser Instruction
setMemP = do
    try $ string "mem["
    addr <- many1 digit
    string "] = "
    val <- many1 digit
    endOfLine
    return $ SetMem (read addr) (read val)

parseProgram = parseOrDie . many1 $ setMemP <|> setMaskP
part1 = memsum . foldl step emptyState
memsum (MState _ m) = sum . map toInteger . elems $ m

step :: MState -> Instruction -> MState
step (MState _ mem) (SetMask mask) = MState mask mem
step (MState mask mem) (SetMem addr val) = MState mask newmem
    where newmem = insert addr (apply mask val) mem

apply2 mask addr = foldl f [addr] ops where
    ops = zip [35,34..0] mask
    f ns (_,Fix0) = ns
    f ns (i,Fix1) = map (`setBit` i) ns
    f ns (i,Noop) = concat . map (setAndClearBit i) $ ns
    setAndClearBit i n = [setBit n i, clearBit n i]

step2 (MState _ mem) (SetMask mask) = MState mask mem
step2 (MState mask mem) (SetMem addr val) = MState mask newmem where
    newmem = foldl memSet mem $ apply2 mask addr
    memSet m a = insert a val m

part2 = memsum . foldl step2 emptyState

main = do
    prog <- parseProgram <$> readFile "day14.txt"
    print $ part1 prog
    print $ part2 prog
