import AOC
import Data.List (foldl')
import Data.List.Split
import Data.IntMap (IntMap, insert, empty, lookup)

data GameState = GameState
    { turn :: Int
    , spoken :: Int
    , previouslySpoken :: IntMap Int
    } deriving Show

playTurn :: GameState -> Int -> GameState
playTurn (GameState t n m) n1 = GameState (t+1) n1 (insert n t m)

seedGame :: [Int] -> GameState
seedGame (x:xs) = foldl playTurn (GameState 1 x empty) xs

step state@(GameState t n m) = playTurn state n1 where
    n1 = case (Data.IntMap.lookup n m) of
        Just x -> t-x
        Nothing -> 0

-- this is awkwardly phrased to avoid storing the whole history of gamestates in memory
-- ...but apparently it still blows up memory anyway.
-- need to revisit this.
nthTurn t1 state@(GameState t0 _ _) = foldl' step' state [t0+1..t1]
    where step' s _ = step s

part1 = spoken . nthTurn 2020 . seedGame
part2 = spoken . nthTurn 3000000 . seedGame

main = do
    seed <- map read . splitOn "," <$> readFile "day15.txt"
    print $ part1 seed
    --print $ part2 seed
