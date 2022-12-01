import qualified Data.ByteString.Char8 as BC8
import qualified Data.Maybe as DM

smooth3 :: [Integer] -> [Integer]
smooth3 xs = zipWith3 (\a b c -> a + b + c)
                      xs
                      (drop 1 xs)
                      (drop 2 xs)

raises :: BC8.ByteString -> Int
raises input = result
  where in_numbers = smooth3 $ (fst . DM.fromJust . BC8.readInteger) <$> BC8.lines input
        result = length $
                   filter (\(a,b)  -> a < b) $
                     zip in_numbers (tail in_numbers)

main :: IO ()
main = do
    -- f <- BC8.readFile "example"
    f <- BC8.readFile "input"
    let ans = raises f
    print $ ans
