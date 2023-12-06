import Data.List

example = [[7, 9], [15, 40], [30, 200]]
input = [[57, 291], [72, 1172], [69, 1176], [92, 2026]]

answers = [f x | f <- [ans1, ans2], x <- [example, input]]

ans1 = product . map solve

ans2 = solve . combine

combine :: [[Int]] -> [Int]
combine = map (read . concat) . transpose . map (map show)

solve :: [Int] -> Int
solve [t, record] = ceiling upper - floor lower - 1
  where
    (lower, upper) = quad 1 (-tF) recordF
    recordF = fromIntegral record
    tF = fromIntegral t

quad a b c = ((-b - d) / (2 * a), (-b + d) / (2 * a))
  where
    d = sqrt (b * b - 4 * a * c)
