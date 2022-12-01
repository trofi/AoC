data Cmd = F Integer
         | D Integer
         | U Integer

solve :: [Cmd] -> Integer
solve input = h * v
  where (h, v) = go input (0, 0)
        go [] (h, v) = (h, v)
        go (c:cs) (h, v) = go cs $ case c of
                          F i -> (h + i, v)
                          D i -> (h    , v + i)
                          U i -> (h    , v - i)

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
