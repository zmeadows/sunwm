module Sunwm.FocusMap where

-- This is essentially just a wrapper around Data.Map
-- that maintains a 'focus'.  All this does is reduce
-- boiler plate code and limit the number of variables
-- needed to keep track of which screen/workspace is focused
-- in sunwm.

import qualified Data.Map as M
import Control.Arrow (second)
import Data.Maybe (fromJust, fromMaybe)

type FocusMap k v = (k, M.Map k v)

focused :: Ord k => FocusMap k v -> v
focused (i,m) = fromJust $ M.lookup i m

unfocused :: Eq a => (a, M.Map a b) -> [b]
unfocused (i,m) =  map snd $ filter (\(k,_) -> k /= i) $ M.toList m

newFocus :: Ord k => k -> FocusMap k v -> FocusMap k v
newFocus n fm@(_,m)
    | elem n $ M.keys m = (n,m)
    | otherwise = fm

(<!>) :: Ord k => FocusMap k v -> k -> v
(<!>) fm@(_,m) n = fromMaybe (focused fm) (M.lookup n m)

updateK :: Ord k => k -> v -> FocusMap k v -> FocusMap k v
updateK n v = second (M.insert n v)

update :: Ord k => v -> FocusMap k v -> FocusMap k v
update v m@(i,_) = updateK i v m

adjustK :: Ord k => k -> (v -> v) -> FocusMap k v -> FocusMap k v
adjustK n f = second (M.adjust f n)

adjust :: Ord k => (v -> v) -> FocusMap k v -> FocusMap k v
adjust f fm@(i,_) = adjustK i f fm

elems :: FocusMap k v -> [v]
elems (_,m) = M.elems m

keys :: FocusMap k v -> [k]
keys (_,m) = M.keys m

fromList :: Ord k => k -> [(k,v)] -> FocusMap k v
fromList focus list
    | focus `elem` ks = (focus, M.fromList list)
    | otherwise = (fst $ head list, M.fromList list)
  where ks = map fst list

mapF :: (v -> v) -> FocusMap k v -> FocusMap k v
mapF f = second (M.map f)

mapElems :: (v -> a) -> FocusMap k v -> [a]
mapElems f = map f . M.elems . snd
