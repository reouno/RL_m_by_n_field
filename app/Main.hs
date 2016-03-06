module Main where

import Lib
import System.Random
--import Text.Printf
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "This is RL005mbyn."
    print $ "environment is " ++ (show.length $ field) ++ " by " ++ (show.length $ field!!0) ++ " field."
    rand <- getStdRandom random :: IO Int
    r1 <- getStdRandom random :: IO Int
    let q = init_Qs (get_actions field (0,0)) rand
    let qs = Map.fromList [((0,0),q)]
    let learned_Qs = episodes 0 field qs r1
    --putStrLn "Learned Q values are followings :"
    --print learned_Qs
    trace <- learned_actions field (0,0) learned_Qs []
    --putStrLn ""
    --print $ "Agent visited " ++ (show.length.Map.keys $ learned_Qs) ++ " places :"
    --print.show.Map.keys $ learned_Qs
    putStrLn ""
    let field_trace = show_field trace $ map (map show) field
    putStrLn "The field is following."
    print_field $ map (map show) field
    putStrLn "Trace of actions after learning of 1000 episodes :"
    print_field field_trace

field :: [[Double]]
field = [[  0,  0,  0,  0,  0],
         [  0,  0,  0,  0,  0],
         [  0,  0,-10,-10,  0],
         [  0,  0,  0,  0,  0],
         [  0,  0,-10,  0,100]]

print_field :: [[[(Char)]]] -> IO ()
print_field field = do
    let loop_i i | i < length field = do
            let row = foldr (++) [] $ map (++ "\\t") (field !! i)
            let format = '"' : row ++ "\""
            putStrLn (read format :: String)
            putStrLn ""
            loop_i $ i + 1
        loop_i _ = return ()
    loop_i 0

--learned_actions :: Foldable t =>
--                   [t a]
--                   -> (Int, Int)
--                   -> Map.Map (Int, Int) (Map.Map Lib.Cross Double)
--                   -> [(Int, Int)]
--                   -> IO [(Int, Int)]
learned_actions field current_position qs trace = do
    r0 <- getStdRandom (randomR (0,0.9999)) :: IO Double
    let action = Map.keys (qs Map.! current_position) !! (take_action 1000 (Map.elems $ qs Map.! current_position) r0)
    let new_position = move current_position action
    let trace' = trace ++ [current_position]
    if new_position == (length field - 1, length (field!!0) - 1)
        then do
            return (trace' ++ [new_position])
        else learned_actions field new_position qs trace'

--step_by1 t field prev_position action qs randNums = do
--    let current_position = move prev_position action
--    let reward = get_reward field current_position
--    let (_, qs') = get_Qs field current_position qs $ round $ (100 * randNums!!0)
--    let qs'' = update_Q prev_position action reward qs' $ randNums!!1
--    let action' = Map.keys (qs'' Map.! current_position) !! (take_action t (Map.elems $ qs'' Map.! current_position) $ randNums!!2)
--    print $ "current_position  =  " ++ show current_position
--    print $ "reward            =  " ++ show reward
--    print $ "updated Q values  =  " ++ show qs''
--    print $ "action  =  " ++ show action'
--    putStrLn ""
--    if t > 100
--        then return ()
--        else step_by1 (t+1) field current_position action' qs'' $ drop 3 randNums


