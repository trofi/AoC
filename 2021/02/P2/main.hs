data Cmd = F Integer
         | D Integer
         | U Integer

solve :: [Cmd] -> Integer
solve input = h * v
  where (h, v, _) = go input (0, 0, 0)
        go [] (h, v, a) = (h, v, a)
        go (c:cs) (h, v, a) =
          go cs $ case c of
             F i -> (h + i, v + a * i, a    )
             D i -> (h    , v        , a + i)
             U i -> (h    , v        , a - i)

readInput :: String -> [Cmd]
readInput inp = (toCmd . words) <$> lines inp
  where toCmd [sc, si] = c i
          where c = case sc of
                      "forward" -> F
                      "down"    -> D
                      "up"      -> U
                i :: Integer
                i = read si

main :: IO ()
main = do
    -- f <- readFile "example"
    f <- readFile "input"
    let ans = solve $ readInput f
    print $ ans
