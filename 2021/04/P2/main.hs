import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.Function as DF
import qualified Data.List as L
import qualified Numeric as N

import qualified Data.List.Split as DLS

type I     = Integer
type Board = [(I, Bool)]

data D = D { input :: [I]
           , boards :: [Board]
           } deriving Show

fWinning :: [Board] -> [Board]
fWinning bs = filter isW bs
    where isW b = any id $ map (all id) $ horis ++ vert
              where bb = map snd b
                    horis = DLS.chunksOf 5 bb
                    vert  = [ [ bb !! (i + 5 * r)
                              | r <- [0..4] ]
                            | i <- [0..4]
                            ]

solve :: D -> Integer
solve d0 = wi * sum [v | (v, False) <- wb]
    where ds = filter (\(_, (D _ bs)) -> not $ L.null bs) $ map (\(i, (D inp bs)) -> (i, D inp $ fWinning bs)) $ L.unfoldr step d0
          step (D []     _ ) = Nothing
          step (D (i:is) bs) = Just ((i, (D is bs')), (D is (bs' L.\\ fWinning bs')))
              where bs' = map mark bs
                    mark b = [ (v, b' || v == i) | (v, b') <- b]
          (wi, (D _ [wb])) = last ds

readInput :: String -> D
readInput inp = D i bs
    where (ri : rbs) = DLS.splitOn "\n\n" inp
          i = map read $ DLS.splitOn "," ri
          bs = map (map (\w -> (read w, False)) . words) rbs

main :: IO ()
main = CM.forM_ ["example", "input"] $ \fn -> do
    f <- readFile fn
    let inp = readInput f
        ans = solve inp
    print $ (fn, ans)
