wonderous :: Int -> [Int]
wonderous n = reverse $ wonderous' [n]

wonderous' :: [Int] -> [Int]
wonderous' n = let next = w_seq $ head n in
  case next of
    1 -> 1 : n
    _ -> wonderous' $ (next : n)
  where w_seq n
          | odd n  = 3 * n + 1
          | even n = n `div` 2


