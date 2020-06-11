module Main where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getX :: Int -> Int
getX n = let dim = isqrt n in (topRight n (dim)) - dim

getY :: Int -> Int
getY n = n - 1

buildMid :: Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
buildMid n x y oGmid prevN = if x == 0 then buildMid n (getX n) (getY n) oGmid prevN else
    case oGmid of
    []          -> error "fuck"
    [r]         -> [[x] ++ r ++ [y]]
    (r : rs)    -> if y == n 
        then [[x],r,[y]] ++ (buildMid n (x - 1) (prevN + 1) rs prevN)
        else [[x],r,[y]] ++ (buildMid n (x - 1) (y + 1) rs prevN)

topRight :: Int -> Int -> Int
topRight n dim = let temp = dim `div` 2 in n - (temp - 1)

build :: Int -> [[ Int ]]
build n = case n of
    1 -> [ [ 1 ] ]
    n -> 
        let dim = isqrt n
            prevN = ((dim) - 2) * ((dim) - 2)
            oGmid = build prevN
            tpRight = topRight n dim
            top = [(tpRight - (dim - 1)) .. tpRight]
            mid = buildMid n 0 0 oGmid prevN
            btLeft = tpRight - 2 * (dim - 1)
            bot = reverse [btLeft - (dim - 1) .. btLeft]
        in [top] ++ mid ++ [bot]

printMatrix :: [[Int]] -> String
printMatrix ans = case ans of
    []       -> ""
    (a : as) -> case a of
        [] -> ""
        (a' : as') -> show a' ++ ", " ++ (printMatrix [as']) ++ printMatrix as

main :: IO ()
main = do
    n <- getLine 
    let z = read n
        ans = build z 
        fnl = printMatrix ans
        in if z /= (isqrt z) * (isqrt z) || z `mod` 2 == 0 then error "Must enter odd perfect square" else putStrLn fnl