import Control.Monad

sqrt' :: Double -> Double
sqrt' x
    | x < 0 = error "負の数の平方根は求められません!" -- 「|」を用いたガード構文
    | x == 0 = 0
    | otherwise = iter (if x > 1 then init 1 x else 1 )
        where
            -- 初期推定値の計算 s = 1から始めてxに近づけていく
            init s x
                | s >= x = s
                | otherwise = init (s * 2) (x / 2)
            -- f(x) = x^2 - a としたときのニュートン法の漸化式
            iter p = let q = (p + x / p) / 2
                        in if q >= p then p else iter q

main :: IO ()
main = do
    putStrLn "数値を入力して！"
    numStr <- getLine
    let num = read numStr
    let result = sqrt' num
    putStrLn ("ルート２は大体：" ++ show result)
