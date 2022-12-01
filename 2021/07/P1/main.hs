{-# OPTIONS_GHC -Wall #-}
import qualified Control.Monad as CM
import qualified Data.List as L

type I = Integer

solve :: [I] -> I
solve is = -- minimum $ map moves [avg - 2 .. avg + 2]
           minimum $ map moves [minimum is .. maximum is]
    where avg = sum is `div` L.genericLength is
          moves target = sum [ abs (target - i) | i <- is ]

readInput :: String -> [I]
readInput = map read . words . map (\c -> if c == ',' then ' ' else c)

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let inp = readInput f
        ans = solve inp
    print $ (fn, ans)
