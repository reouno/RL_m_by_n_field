module Main where

import Lib
import System.Random.Mersenne.Pure64 (newPureMT, randomWord64, randomDouble)
import Data.Word (Word64)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vec
import Data.Time (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    putStrLn "This is RL005mbyn."
    print $ "environment is " ++ (show.length $ field) ++ " by " ++ (show.length $ field Vec.! 0) ++ " field."
-- mt-pure64を使う
    gen <- newPureMT
    let (r0, gen') = randomWord64 gen
    let (r1, _) = randomWord64 gen'
    let q = init_Qs' (get_actions field (0,0)) r0
    let qs = Map.fromList [((0,0),q)]
    let learned_Qs = episodes' 0 field qs r1
    trace <- learned_actions field (0,0) learned_Qs []
    let field_trace = show_field trace $ Vec.map (Vec.map show) field
    putStrLn "The field is following."
    print_field $ Vec.map (Vec.map show) field
    putStrLn "Trace of actions after learning of 1000 episodes :"
    print_field field_trace

-- 以下の5行は学習成功率測定用（ついでに時間も計る）
--    start <- getCurrentTime
--    rate <- success_rate 100
--    end <- getCurrentTime
--    print $ "success rate of learning is " ++ show rate
--    print $ "processing time is " ++ show (diffUTCTime end start) ++ " sec."

field :: Field Double
field = Vec.fromList [Vec.fromList [  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
                      Vec.fromList [  0,  0,  0,  0,  0,  0,  0,  0,-10,  0],
                      Vec.fromList [  0,  0,-10,  0,  0,-10,  0,  0,  0,  0],
                      Vec.fromList [  0,  0,  0,  0,  0,  0,  0,  0,-10,  0],
                      Vec.fromList [  0,  0,-10,  0,  0,-10,  0,  0,  0,  0],
                      Vec.fromList [  0,  0,  0,  0,-10,  0,  0,-10,-10,  0],
                      Vec.fromList [  0,-10,  0,  0,  0,  0,  0,  0,  0,  0],
                      Vec.fromList [  0,  0,  0,  0,  0,  0,  0,  0,-10,  0],
                      Vec.fromList [  0,-10,  0,-10,  0,  0,-10,  0,  0,  0],
                      Vec.fromList [  0,  0,  0,  0,  0,  0,  0,  0,  0,100]]

print_field :: Field [Char] -> IO ()
print_field field = do
    let loop_i i | i < length field = do
            let row = Vec.foldr (++) [] $ Vec.map (++ "\\t") (field Vec.! i)
            let format = '"' : row ++ "\""
            putStrLn (read format :: String)
            putStrLn ""
            loop_i $ i + 1
        loop_i _ = return ()
    loop_i 0

learned_actions :: Foldable t
                   => Vec.Vector(t a)
                   -> (Int, Int)
                   -> VValues (Int, Int) (QValues Lib.Cross Double)
                   -> [(Int, Int)]
                   -> IO [(Int, Int)]
learned_actions field current_position qs trace = do
    gen <- newPureMT
    let (rand, _) = randomDouble gen
    let action = Map.keys (qs Map.! current_position) !! (take_action 1000 (Map.elems $ qs Map.! current_position) rand)
    let new_position = move current_position action
    let trace' = trace ++ [current_position]
    if new_position == (length field - 1, length (field Vec.! 0) - 1)
        then do
            return (trace' ++ [new_position])
        else learned_actions field new_position qs trace'

learn_1000 :: IO Int
learn_1000 = do
    gen <- newPureMT
    let (r0, gen') = randomWord64 gen
    let (r1, _) = randomWord64 gen'
    let q = init_Qs' (get_actions field (0,0)) r0
    let qs = Map.fromList [((0,0),q)]
    let learned_Qs = episodes' 0 field qs r1
    trace <- learned_actions field (0,0) learned_Qs []
    return $ length trace

success_rate :: (Fractional r, Ord r) => r -> IO r
success_rate n = do
    let 
        loop i sum_i | i < n = do
            num <- learn_1000
            --print num
            if num == length field + length (field Vec.! 0) - 1
                then do
                    --print $ "success rate is " ++ show (sum_i + 1) ++ "/" ++ show n
                    loop (i + 1) (sum_i + 1)
                else do
                    --print $ "success rate is " ++ show sum_i ++ "/" ++ show n
                    loop (i + 1) sum_i
        loop _ sum_i = return $ sum_i / n
    loop 0 0
-- n回episodesを実行して、学習成功率を返す。
