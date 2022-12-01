
import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.Function as DF
import qualified Data.List as L
import qualified Numeric as N

solve :: [String] -> Integer
solve input = rB oxygen * rB co2
  where oxygen = pick last 0 input
        co2    = pick head 0 input
        pick _  _ [v] = v
        pick f ix inp = pick f (succ ix) [ i
                                         | i <- inp
                                         , let c = snd
                                                 $ f
                                                 $ L.sort
                                                 $ map (\l -> (length l, head l))
                                                 $ L.group
                                                 $ L.sort
                                                 $ map (!! ix) inp
                                         , i !! ix == c
                                         ]

-- readBin
rB :: String -> Integer
rB = fst . head . N.readInt 2 (`elem` "01") DC.digitToInt

readInput :: String -> [String]
readInput inp = lines inp

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let ans = solve $ readInput f
    print $ (fn, ans)
