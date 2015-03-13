import System.Environment
import Data.List
import Data.Ratio

type ScurryInfo = (Int, Int)
type Number = Rational
gap :: Number -> Number -> Number -> Number
gap blockCount blockSize totalSize =  (totalSize - blockCount * blockSize) / (blockCount - 1)


block blockCount blockSize totalSize n = [floor start .. floor end]
    where
        start = 0 + n * (blockSize + gap blockCount blockSize totalSize)
        end =  start + blockSize - 1


result blockCount blockSize totalSize = input ++ replicate (15 - length input) ' ' ++ show output
    where
        input = show (floor$blockCount, floor$blockSize, floor$totalSize)
        output = map (\n ->  block blockCount blockSize totalSize n ) [0..blockCount - 1]
{-
        blockSize = 1
        blockCount = 3
        totalSize = 8
-}

main = do 
    input <- getArgs
    blockCount <- return 4
    blockSize  <- return 20
    totalSize  <- return 7

    --[blockCount, blockSize, totalSize] <- return [3,4,5]
 --   [blockCount, blockSize, totalSize] <- return $ map (toRational.(read::(String->Int))) input
--    putStrLn map ($  input
--    mapM (putStr.(++" ").show) [blockCount, blockSize, totalSize]
    putStrLn $ result blockCount blockSize totalSize

--main = print $ result 
-- intersect [1..10] [10..1000]

