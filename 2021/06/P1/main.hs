{-# OPTIONS_GHC -Wall #-}
import qualified Control.Monad as CM
import qualified Data.Map as DM
import qualified Data.Maybe as M

type I = Integer

solve :: [I] -> I
solve = go 80 . DM.fromListWith (+) . map (\v -> (v, 1))
    where go :: I -> DM.Map I I -> I
          go 0 s = sum $ DM.elems s
          go n s = go (n - 1) (step s)

          tick :: I -> I
          tick 0 = 6
          tick n = n -1

          step :: DM.Map I I -> DM.Map I I
          step s = DM.fromListWith (+) $ (8,nn):so
              where sl = DM.toList s
                    so = map (\(k,v) -> (tick k, v)) sl
                    nn = M.maybe 0 id $ DM.lookup 0 s

readInput :: String -> [I]
readInput = map read . words . map (\c -> if c == ',' then ' ' else c)

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let inp = readInput f
        ans = solve inp
    print $ (fn, ans)
