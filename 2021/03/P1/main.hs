import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.Function as DF
import qualified Data.List as L
import qualified Numeric as N

solve :: [String] -> Integer
solve input = rB gamma * rB eps
    where trin = tr input
          gamma = map (pick last) trin
          eps   = map (pick head) trin
          -- sort by popularity
          pick f = f
                 . map snd
                 . L.sortBy (compare `DF.on` fst)
                 . map (\l -> (length l, head l))
                 . L.group
                 . L.sort

-- transpose:
-- ["123", "abc", "xyz"] -> ["1ax", "2by", "3cz"]
tr :: [String] -> [String]
tr ins | all L.null ins = []
tr ins = map head ins : tr (map tail ins)

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
