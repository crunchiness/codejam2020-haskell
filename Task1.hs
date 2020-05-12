-- http://brandon.si/code/haskell-boilerplate-for-google-codejam/
-- https://github.com/dradtke/Code-Jam/blob/master/alien-language/alien-language.hs
-- https://github.com/dradtke/Code-Jam/blob/master/minimum-scalar/minimum-scalar.hs
import Control.Monad

main = readLn >>= \n -> mapM solve [1..n]

solve :: Int -> IO ()
solve i = do
    nLines <- readLn :: IO Integer
    matrix <- mapM (\x -> readIntList) [1..nLines]
    putStrLn (show i)
    putStrLn (show matrix)

-- Get the next n lines from standard input
getLines :: Int -> IO [String]
getLines n = foldM f [] [1..n]
    where f l _ = getLine >>= \x -> return $ x:l

-- Read a list of Int's from standard input
readIntList :: IO [Int]
readIntList = getLine >>= \l -> return (fmap read $ words l)
