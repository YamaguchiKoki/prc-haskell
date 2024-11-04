import Control.Monad

main :: IO ()
main = do
    putStrLn "いくつまで !"
    numStr <- getLine
    let num = read numStr -- readは文字列を数値に変換する
    fizzBuzz num

fizzBuzz :: (Show a, Integral a) => a -> IO ()
fizzBuzz num  = do
    forM_ [1..num] $ \i -> do
        putStrLn (show i ++ ": " ++ toFizzBuzz i)

toFizzBuzz :: (Show a, Integral a) => a -> String
toFizzBuzz num =
    case mod num 15 of
        0 -> "FizzBuzz"
        3 -> "Fizz"
        5 -> "Buzz"
        6 -> "Fizz"
        9 -> "Fizz"
        10 -> "Buzz"
        12 -> "Fizz"
        _ -> ""
