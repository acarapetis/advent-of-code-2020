getInts :: IO [Integer]
getInts = fmap ((map read) . lines) getContents

part1 inputs = [ x*y | x <- inputs, y <- inputs, x+y == 2020 ] !! 0
part2 inputs = [ x*y*z | x <- inputs, y <- inputs, z <- inputs, x+y+z == 2020 ] !! 0

main = do
    input <- getInts
    putStrLn . show $ part1 input
    putStrLn . show $ part2 input
