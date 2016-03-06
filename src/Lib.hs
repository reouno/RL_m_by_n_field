module Lib
    ( get_actions
    , init_Qs
    , get_Qs
    , temper, calc_p, choose_action, take_action
    , choose_action'
    --, elemN, split2list, elems_list
    , move
    , get_reward
    , update_Q
    , episode
    , episodes
    , as
    , replace_mn
    , show_field
    ) where

import System.Random
import qualified Data.Map as Map

-- データ型定義
data Cross = ToUp | ToDown | ToLeft | ToRight
              deriving (Eq, Show, Ord)

-- 定数
alpha :: Double
alpha = 0.2

gamma :: Double
gamma = 0.9


-- 関数
get_barriers :: Foldable t => [t a] -> (Int, Int) -> [Lib.Cross]
get_barriers field current_position = up ++ down ++ left ++ right
    where
        up = if fst current_position == 0 then [ToUp] else []
        down = if fst current_position == length field - 1 then [ToDown] else []
        left = if snd current_position == 0 then [ToLeft] else []
        right = if snd current_position == length (field!!0) - 1 then [ToRight] else []
-- 移動不可能な方向のリストを作成（get_actions定義内で使用する）

get_actions :: Foldable t => [t a] -> (Int, Int) -> [Lib.Cross]
get_actions field current_position = filter (flip notElem barriers) [ToUp, ToDown, ToLeft, ToRight]
    where barriers = get_barriers field current_position
-- 移動可能な方向のリストを作成

init_Qs :: Ord k => [k] -> Int -> Map.Map k Double
init_Qs actions randNum = Map.fromList $ zip actions randNums
    where randNums = randomRs (0, 0.001) $ mkStdGen randNum :: [Double]
-- 可能な行動のQ値を新たに生成する

get_Qs :: Foldable t =>
                [t a]
                -> (Int, Int)
                -> Map.Map (Int, Int) (Map.Map Lib.Cross Double)
                -> Int
                -> (Map.Map Lib.Cross Double,
                    Map.Map (Int, Int) (Map.Map Lib.Cross Double))
get_Qs field current_position qs randNum =
    if Map.member current_position qs == True
        then (qs Map.! current_position, qs)
        else (qs_init, Map.insert current_position qs_init qs)
            where qs_init = init_Qs (get_actions field current_position) randNum

temper :: Integral a => a -> Double
temper t = 1 / log(0.01 * ((fromIntegral t) :: Double) + 1.1)
-- 温度（temperture）の関数、と言ってもここでは、十分な時間が経過すると0に収束する関数として1/logxを使った

map2List qs = Map.elems qs

calc_p :: Floating b => [b] -> b -> [b]
calc_p qs temp = map (/ denom) numer
    where
        numer = (map exp) . (map (/temp)) $ qs
        denom = sum numer

choose_action :: (Num a, Ord a) => [a] -> a -> Int
choose_action ps randNum =
        let
            ps_next = sum_reduce ps
            sum_reduce [] = []
            sum_reduce [x] = [x]
            sum_reduce (x0:x1:xs) = (x0 + x1) : xs
            choose_action' ps@(x0:_) len_ps randNum
                | randNum <= x0 = len_ps - length ps
                | randNum >  1  = error "randNum must be 1 or less."
                | otherwise     = choose_action' ps_next len_ps randNum
        in
            choose_action' ps (length ps) randNum

choose_action' ps@(x0:_) len_ps randNum
    | randNum <= x0 = len_ps - length ps
    | randNum >  1  = error "randNum must be 1 or less."
    | otherwise     = choose_action' ps_next len_ps randNum
        where
            ps_next = sum_reduce ps
            sum_reduce [] = []
            sum_reduce (x0:x1:xs) = (x0 + x1) : xs

int2Cross i qs = Map.keys qs !! i

take_action :: Integral a => a -> [Double] -> Double -> Int
take_action t qs randNum = choose_action' (calc_p qs $ temper t) (length qs) randNum

--elemN :: (Eq b, Eq a1, Num b, Num a, Num a1) => (a1, b) -> a1 -> b -> a
--elemN size m n =
--    let
--        --(m, n) = mn
--        m_max = fst size - 1
--        n_max = snd size - 1
--    in
--        if (m==0 || m==m_max) && (n==0 || n==n_max)
--            then 2
--        else if ((m==0 || m==m_max) && (n/=0) && (n/=n_max)) || ((n==0 || n==n_max) && (m/=0) && (m/=m_max))
--            then 3
--            else 4
--
--elems_list :: (Enum b, Eq b, Num t, Num b, Num a, Ord a) => (a, b) -> [t]
--elems_list size = elems_list' size 0
--    where
--        elems_list' size m_0 =
--            if m_0 > fst size - 1
--                then []
--                else map (elemN size m_0) [0..snd size - 1] ++ elems_list' size (m_0+1)
--
--split2list :: [Int] -> [a] -> [[a]]
--split2list [] _     = []
--split2list elems xs = xs_fst : split2list elems' xs_snd
--    where
--        (xs_fst, xs_snd) = splitAt (head elems) xs
--        elems' = drop 1 elems

move :: (Num b, Num a) => (a, b) -> Lib.Cross -> (a, b)
move current_position action
    | action == ToUp   = (fst current_position - 1, snd current_position)
    | action == ToDown = (fst current_position + 1, snd current_position)
    | action == ToLeft = (fst current_position, snd current_position - 1)
    | otherwise        = (fst current_position, snd current_position + 1)
-- 座標を移動する（状態遷移）

get_reward :: [[a]] -> (Int, Int) -> a
get_reward field current_position = field!!(fst current_position)!!(snd current_position)

update_Q :: (Num a, Num b, Ord a, Ord b) =>
            (a, b)
            -> Lib.Cross
            -> Double
            -> Map.Map (a, b) (Map.Map Lib.Cross Double)
            -> t
            -> Map.Map (a, b) (Map.Map Lib.Cross Double)
update_Q prev_position action reward qs randNum = Map.alter new_qs prev_position qs
    where
        new_qs _ = Just (Map.alter new_qs' action $ qs Map.! prev_position)
        new_qs' _ = Just (q_prev + alpha * (reward + gamma * max_qs - q_prev))
        q_prev = qs Map.! prev_position Map.! action
        max_qs = Map.foldr max 0 $ qs Map.! (move prev_position action)

episode t field prev_position action qs randNums =
    let
        current_position = move prev_position action
        reward = get_reward field current_position
        (_, qs') = get_Qs field current_position qs $ round $ (100 * randNums!!0)
        qs'' = update_Q prev_position action reward qs' $ randNums!!1
        action' = Map.keys (qs'' Map.! current_position) !! (take_action t (Map.elems $ qs'' Map.! current_position) $ randNums!!2)

    in
        if reward > 0
            then (t+1, qs'')
            else episode (t+1) field current_position action' qs'' $ drop 3 randNums

episodes count field qs randNum =
    let
        randNums = randomRs (0,0.9999) $ mkStdGen randNum :: [Double]
        action = Map.keys (qs Map.! (0,0)) !! (take_action count (Map.elems $ qs Map.! (0,0)) $ randNums!!0)
        (_, qs') = episode count field (0,0) action qs $ drop 1 randNums
    in
        if count > 999
            then qs'
            else episodes (count+1) field qs' $ round $ 100 * randNums!!0

as = [ToUp, ToDown, ToLeft, ToRight]

replace_mn :: (Eq a, Num a) => Int -> a -> t -> [[t]] -> [[t]]
replace_mn m n new_value xs = replace_n m (replace_n n new_value $ xs!!m) xs
    where
        replace_n _ _ [] = []
        replace_n 0 new_value (x:xs) = new_value:xs
        replace_n n new_value (x:xs) = x:replace_n (n - 1) new_value xs

show_field (_:trace) field =
    if length trace > 1
        then show_field trace $ replace_mn (fromIntegral.fst.head $ trace :: Int) (snd.head $ trace) " + " field
        else field''
            where
                field' = replace_mn 0 0 "Start" field
                field'' = replace_mn ((fromIntegral.length) field' - 1 :: Int) ((length.head $ field') - 1) "Goal" field'


