import Control.Monad
import Text.Printf

main = readLn >>= \n -> mapM solve [1..n]

hasDup :: [Int] -> Bool
hasDup xs = f xs []
    where
        f [] _           = False
        f (x:xs) visited =
            if any (== x) visited
            then True
            else f xs (x:visited)

trace :: [[Int]] -> Int
trace [] = 0
trace ((x:xs):ys) = x + trace (map tail ys)

transpose :: [[Int]] -> [[Int]]
transpose [] = []
transpose ([]:xs) = []
transpose m = (map head m):(transpose $ map tail m)

doVestigium :: [[Int]] -> (Int, Int, Int)
doVestigium matrix = (trace matrix, rowDups matrix, colDups matrix)
    where
        rowDups m = sum $ map (\x -> if hasDup x then 1 else 0) m
        colDups m = rowDups (transpose m)

solve :: Int -> IO ()
solve i = do
    nLines <- readLn :: IO Int
    matrix <- mapM (\x -> readIntList) [1..nLines]
    let (k, r, c) = doVestigium matrix
    putStrLn $ printf "Case #%d: %d %d %d" i k r c

-- Get the next n lines from standard input
getLines :: Int -> IO [String]
getLines n = foldM f [] [1..n]
    where f l _ = getLine >>= \x -> return $ x:l

-- Read a list of Int's from standard input
readIntList :: IO [Int]
readIntList = getLine >>= \l -> return (fmap read $ words l)
