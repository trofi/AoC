{-# OPTIONS_GHC -Wall #-}
import qualified Control.Monad as CM

type I = Integer

solve :: [I] -> I
solve is = minimum $ map moves [minimum is .. maximum is]
    where moves target = sum [ delta * (delta + 1) `div` 2 | i <- is, let delta = abs (target - i)]

readInput :: String -> [I]
readInput = map read . words . map (\c -> if c == ',' then ' ' else c)

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let inp = readInput f
        ans = solve inp
    print $ (fn, ans)
