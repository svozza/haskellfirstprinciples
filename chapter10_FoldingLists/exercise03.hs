fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 $ fibs

fibsUnder100 = filter (\x -> x < 100) fibs20

factorial' :: Int -> Int
factorial' n = (scanl (*) 1 [1..n]) !! n


