import qualified Data.ByteString.Char8 as BC8
import qualified Data.Maybe as DM

raises :: BC8.ByteString -> Int
raises input = result
  where in_numbers = (fst . DM.fromJust . BC8.readInteger) <$> BC8.lines input
        result = length $
                   filter (\(a,b)  -> a < b) $
                     zip in_numbers (tail in_numbers)

main :: IO ()
main = do
    -- f <- BC8.readFile "example"
    f <- BC8.readFile "input"
    let ans = raises f
    print $ ans
