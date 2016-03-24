module Main where

import Lib
import System.Random
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vec
import Data.Time (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    putStrLn "This is RL005mbyn."
    print $ "environment is " ++ (show.length $ field) ++ " by " ++ (show.length $ field Vec.! 0) ++ " field."
    rand <- getStdRandom random :: IO Int
    r1 <- getStdRandom random :: IO Int
    let q = init_Qs (get_actions field (0,0)) rand
    let qs = Map.fromList [((0,0),q)]
    let learned_Qs = episodes 0 field qs r1
    --putStrLn "Learned Q values are followings :"
    --print learned_Qs
    trace <- learned_actions field (0,0) learned_Qs []
    let field_trace = show_field trace $ Vec.map (Vec.map show) field
    putStrLn "The field is following."
    print_field $ Vec.map (Vec.map show) field
    putStrLn "Trace of actions after learning of 1000 episodes :"
    print_field field_trace

-- 以下の5行は学習成功率測定用
--    start <- getCurrentTime
--    rate <- success_rate 100
--    end <- getCurrentTime
--    print $ "success rate of learning is " ++ show rate
--    print $ "processing time is " ++ show (diffUTCTime end start) ++ " sec."

field :: Vec.Vector (Vec.Vector Double)
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

print_field :: Vec.Vector (Vec.Vector [Char]) -> IO ()
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
                   -> Map.Map (Int, Int) (Map.Map Lib.Cross Double)
                   -> [(Int, Int)]
                   -> IO [(Int, Int)]
learned_actions field current_position qs trace = do
    r0 <- getStdRandom (randomR (0,0.9999)) :: IO Double
    let action = Map.keys (qs Map.! current_position) !! (take_action 1000 (Map.elems $ qs Map.! current_position) r0)
    let new_position = move current_position action
    let trace' = trace ++ [current_position]
    if new_position == (length field - 1, length (field Vec.! 0) - 1)
        then do
            return (trace' ++ [new_position])
        else learned_actions field new_position qs trace'

learn_1000 :: IO Int
learn_1000 = do
    rand <- getStdRandom random :: IO Int
    r1 <- getStdRandom random :: IO Int
    let q = init_Qs (get_actions field (0,0)) rand
    let qs = Map.fromList [((0,0),q)]
    let learned_Qs = episodes 0 field qs r1
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

-- 以下の2つの処理は、デバッグ用に作ったものだから、main実行には必要ない
--episode_io :: Int
--              -> Vec.Vector (Vec.Vector Double)
--              -> (Int, Int)
--              -> Lib.Cross
--              -> Map.Map (Int, Int) (Map.Map Lib.Cross Double)
--              -> [Double]
--              -> IO (Int, Map.Map (Int, Int) (Map.Map Lib.Cross Double))
--episode_io t field prev_position action qs randNums = do
--    let current_position = move prev_position action
--    let reward = get_reward field current_position
--    let (_, qs') = get_Qs field current_position qs $ round $ (100 * randNums!!0)
--    let qs'' = update_Q prev_position action reward qs' $ randNums!!1
--    let possible_actions = Map.keys (qs'' Map.! current_position)
--    let action_qs = Map.elems $ qs'' Map.! current_position
--    let probs = calc_p action_qs $ temper (t :: Int)
--    let action_index = take_action (t :: Int) (Map.elems $ qs'' Map.! current_position) $ randNums!!2
--    let action' = Map.keys (qs'' Map.! current_position) !! (take_action (t :: Int) (Map.elems $ qs'' Map.! current_position) $ randNums!!2)
--    if t > 93
--        then do
--            print $ "current_position = " ++ show current_position
--            print $ "reward = " ++ show reward
--            print $ "possible_actions = " ++ show possible_actions
--            print $ "action_qs = " ++ show action_qs
--            print $ "probs = " ++ show probs
--            print $ "action_index = " ++ show action_index 
--            print $ "action' = " ++ show action'
--        else return ()
--    print current_position
--    if current_position == (length field - 1, length (field Vec.! 0) - 1) -- coordinates of the Goal
--        then return (t+1, qs'')
--        else episode_io (t+1) field current_position action' qs'' $ drop 3 randNums
--
--step_by_step :: Int
--                -> Vec.Vector (Vec.Vector Double)
--                -> Map.Map (Int, Int) (Map.Map Lib.Cross Double)
--                -> IO ()
--step_by_step t field qs = do
--    rand <- getStdRandom random :: IO Int
--    let randNums = randomRs (0, 0.9999) $ mkStdGen rand :: [Double]
--    let action = Map.keys (qs Map.! (0,0)) !! (take_action t (Map.elems $ qs Map.! (0,0)) $ randNums!!0)
--    (times_of_actions, qs') <- episode_io t field (0, 0) action qs $ drop 1 randNums
--    print $ times_of_actions - t
--    print $ "t = " ++ show t
--    if t > 100
--        then return ()
--        else step_by_step (t+1) field qs'