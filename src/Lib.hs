module Lib
    ( Field
    , VValues
    , QValues
    , Cross
    , get_barriers, randomDoubles
    , get_actions
    , init_Qs'
    , get_Qs'
    , temper
    , calc_p
    , take_action
    , move
    , get_reward
    , update_Q
    , episodes'
    , show_field
    ) where

import System.Random.Mersenne.Pure64 (PureMT, pureMT, randomDouble, randomWord64)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

type Field a = Vec.Vector (Vec.Vector a) -- 2次元のフィールド
type VValues k v = Map.Map k v -- 状態価値
type QValues k v = Map.Map k v -- 行動価値

-- データ型定義
data Cross = ToUp | ToDown | ToLeft | ToRight
              deriving (Eq, Show, Ord)

-- 定数
alpha :: Double -- 学習率
alpha = 0.2

gamma :: Double -- 割引率
gamma = 0.9

-- 擬似乱数生成関数
randomDoubles :: PureMT -> [Double]
randomDoubles gen = rand : randomDoubles gen'
    where (rand, gen') = randomDouble gen

randomWord64s :: PureMT -> [Word64]
randomWord64s gen = rand : randomWord64s gen'
    where (rand, gen') = randomWord64 gen

-- 関数
get_barriers :: Foldable t => Vec.Vector (t a) -> (Int, Int) -> [Cross]
get_barriers field current_position = up ++ down ++ left ++ right
    where
        up = if fst current_position == 0 then [ToUp] else []
        down = if fst current_position == length field - 1 then [ToDown] else []
        left = if snd current_position == 0 then [ToLeft] else []
        right = if snd current_position == length (field Vec.!0) - 1 then [ToRight] else []
-- 移動不可能な方向のリストを作成（get_actions定義内で使用する）

get_actions :: Foldable t => Vec.Vector (t a) -> (Int, Int) -> [Cross]
get_actions field current_position = filter (flip notElem barriers) [ToUp, ToDown, ToLeft, ToRight]
    where barriers = get_barriers field current_position
-- 移動可能な方向のリストを作成

init_Qs' :: Ord k => [k] -> Word64 -> QValues k Double
init_Qs' actions randNum = Map.fromList $ zip actions randNums
    where randNums = map (/1000) $ randomDoubles $ pureMT randNum
-- 可能な行動のQ値を新たに生成する
-- randNumsの各要素は、[0, 0.001)の範囲

get_Qs' :: Foldable t
           => Vec.Vector (t a)
           -> (Int, Int)
           -> VValues (Int, Int) (QValues Cross Double)
           -> Word64
           -> (QValues Cross Double,
               VValues (Int, Int) (QValues Cross Double))
get_Qs' field current_position qs randNum =
    if Map.member current_position qs == True
        then (qs Map.! current_position, qs)
        else (qs_init, Map.insert current_position qs_init qs)
            where qs_init = init_Qs' (get_actions field current_position) randNum
-- randNumはWord64型

temper :: Integral a => a -> Double
temper t = 1 / log(0.1 * ((fromIntegral t) :: Double) + 1.1) + 0.1
-- 温度（temperture）の関数、と言ってもここでは、十分な時間が経過すると0に収束する関数として1/logxを使った
-- そして、これが0になるとまずいので、最小値を0.1にした

calc_p :: Floating b => [b] -> b -> [b]
calc_p qs temp = map (/ denom) numer
    where
        numer = (map exp) . (map (/temp)) $ qs
        denom = sum numer

choose_action ps@(x0:_) len_ps randNum
    | randNum <= x0 = len_ps - length ps
    | randNum >  1  = error "randNum must be 1 or less."
    | otherwise     = choose_action ps_next len_ps randNum
        where
            ps_next = sum_reduce ps
            sum_reduce [] = []
            sum_reduce [x] = [x]
            sum_reduce (x0:x1:xs) = (x0 + x1) : xs
-- randNumは、[0, 1)の範囲

normalize_Qs :: (Fractional b, Ord b) => [b] -> [b]
normalize_Qs qs = if maximum qs' > 10 then map (\x -> x*10 / maximum qs') qs' else qs'
    where
        qs' = map (\x -> x - maximum qs + minimum qs) qs -- lambda = x - (max - min)
-- take_actionの引数に入れる時にQ値を正規化する
-- まず、MaxとMinの中間の値を見つけてそれを0とする（すべての要素からその値を引く）
-- これで、Max = - Min となった
-- 次に、もしMax > 10なら、Max == 10となるように正規化する

take_action :: Integral a => a -> [Double] -> Double -> Int
take_action t qs randNum = choose_action (calc_p qs' $ temper t) (length qs) randNum
    where qs' = normalize_Qs qs
-- randNumは、[0, 1)の範囲

move :: (Num b, Num a) => (a, b) -> Cross -> (a, b)
move current_position action
    | action == ToUp   = (fst current_position - 1, snd current_position)
    | action == ToDown = (fst current_position + 1, snd current_position)
    | action == ToLeft = (fst current_position, snd current_position - 1)
    | otherwise        = (fst current_position, snd current_position + 1)
-- 座標を移動する（状態遷移）

get_reward :: Field a -> (Int, Int) -> a
get_reward field current_position = field Vec.! (fst current_position) Vec.! (snd current_position)

update_Q :: (Num a, Num b, Ord a, Ord b) =>
            (a, b)
            -> Cross
            -> Double
            -> VValues (a, b) (QValues Cross Double)
            -> VValues (a, b) (QValues Cross Double)
update_Q prev_position action reward qs = Map.alter new_qs prev_position qs
    where
        new_qs _ = Just (Map.alter new_qs' action $ qs Map.! prev_position)
        new_qs' _ = Just (q_prev + alpha * (reward + gamma * max_qs - q_prev))
        q_prev = qs Map.! prev_position Map.! action
        max_qs = Map.foldr max 0 $ qs Map.! (move prev_position action)
-- Q値の更新にたぶん時間がかかっているから、ここだけ破壊的に行えれば、速くなりそう

episode' :: Integral a =>
           a
           -> Field Double
           -> (Int, Int)
           -> Cross
           -> VValues (Int, Int) (QValues Cross Double)
           -> [Word64]
           -> (a, VValues (Int, Int) (QValues Cross Double))
episode' t field prev_position action qs randNums =
    let
        current_position = move prev_position action
        reward = get_reward field current_position
        (_, qs') = get_Qs' field current_position qs $ randNums!!0
        qs'' = update_Q prev_position action reward qs'
        action' = Map.keys (qs'' Map.! current_position) !! (take_action t (Map.elems $ qs'' Map.! current_position) $ fromIntegral (randNums!!1) / (2^64 - 1))
    in
        if reward > 0
            then (t+1, qs'')
            else episode' (t+1) field current_position action' qs'' $ drop 2 randNums
-- これは、START(0, 0)から行動をはじめて、正の報酬を得るまで行動し続ける関数
-- randNumsは[Word64]型

episodes' :: Integral t =>
            t
            -> Field Double
            -> VValues (Int, Int) (QValues Cross Double)
            -> Word64
            -> VValues (Int, Int) (QValues Cross Double)
episodes' count field qs randNum =
    let
        randNums = randomWord64s $ pureMT randNum
        action = Map.keys (qs Map.! (0,0)) !! (take_action count (Map.elems $ qs Map.! (0,0)) $ fromIntegral (randNums!!0) / (2^64 - 1))
        (_, qs') = episode' count field (0,0) action qs $ drop 2 randNums
    in
        if count > 999
            then qs'
            else episodes' (count+1) field qs' $ randNums!!1

show_field :: Integral a =>
               [(a, Int)]
               -> Field [Char]
               -> Field [Char]
show_field (_:trace) field =
    if length trace > 1
        then show_field trace $ field Vec.// [(fromIntegral.fst.head $ trace :: Int, (field Vec.! (fromIntegral.fst.head $ trace :: Int)) Vec.// [(snd.head $ trace, " . ")])]
        else field'
            where
                field' = field Vec.// [(0, (field Vec.! 0) Vec.// [(0, "START")]), (9, (field Vec.! 9) Vec.// [(9, "GOAL")])]
