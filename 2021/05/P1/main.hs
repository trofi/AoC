{-# OPTIONS_GHC -Wall #-}
import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.List as DL

type I = Integer

data L = H I I I
       | V I I I
       | Unsupported

solve :: [L] -> Integer
solve = DL.genericLength
      . filter (\g -> length g > 1)
      . DL.group
      . DL.sort
      . concatMap expand
    where expand (H y x1 x2) = [ (x,y) | x <- [x1..x2]]
          expand (V x y1 y2) = [ (x,y) | y <- [y1..y2]]
          expand Unsupported = []

readInput :: String -> [L]
readInput inp = parseL <$> lines inp
    where parseL s = norm ((x1, y1), (x2, y2))
              where [x1,y1,x2,y2] = read <$> words (map (\c -> if DC.isDigit c then c else ' ') s)
          norm ((x1, y1), (x2, y2)) = case () of
            _ | x1 == x2 -> V x1 (min y1 y2) (max y1 y2)
            _ | y1 == y2 -> H y1 (min x1 x2) (max x1 x2)
            -- _            -> error $ "Unexpected line: " ++ show l
            _            -> Unsupported

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let inp = readInput f
        ans = solve inp
    print $ (fn, ans)
