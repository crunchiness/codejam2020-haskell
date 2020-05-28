import Text.Printf
import Data.List (sortBy)
import Data.Function (on)

main = readLn >>= \n -> mapM solve [1..n]

overlap' :: (Int, Int) -> (Int, Int) -> Bool
overlap' (s1, e1) (s2, e2) | e1 == s2 || e2 == s1 = False
                           | otherwise            = s1 <= e2 && s2 <= e1

overlap :: (Int, Int) -> [(Int, Int)] -> Bool
overlap x xs = or $ map (overlap' x) xs

f :: [(Int, (Int, Int))] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Char)] -> String
f [] cs js seq = snd $ unzip $ sortBy (compare `on` fst) seq
f ((i, a):as) cs js seq | not (overlap a cs) = f as (a:cs) js (seq ++ [(i, 'C')])
                        | not (overlap a js) = f as cs (a:js) (seq ++ [(i, 'J')])
                        | otherwise          = "IMPOSSIBLE"

doParentingPartneringReturns :: [(Int, Int)] -> String
doParentingPartneringReturns activities = f (sortBy (compare `on` snd) (zip [1..] activities)) [] [] []

solve :: Int -> IO ()
solve i = do
    nLines <- readLn :: IO Int
    activities <- mapM (\x -> readTuple) [1..nLines]
    let solution = doParentingPartneringReturns activities
    putStrLn $ printf "Case #%d: %s" i solution

readTuple :: IO (Int, Int)
readTuple = do
    line <- getLine
    let s:e:[] = words line
    return (read s, read e)
