{-
  Copyright 2011-2012 Zac Meadows

  This file is part of sunWM.

  sunWM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  sunWM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with sunWM.  If not, see <http://www.gnu.org/licenses/>.
-}

import STree
import Test.QuickCheck
import Control.Category
import Prelude hiding ((.), id)
import Data.Label
import Data.Maybe (isNothing)

instance Arbitrary SUNState where
  arbitrary = do
    wins <- Test.QuickCheck.resize 100 (listOf $ elements [1..99999])
    a <- listOf $ elements treeOps
    b <- listOf $ elements wsOps
    c <- listOf $ elements ssOps
    let winOps = map (modify (tree . focusWS) . replace . Just) wins
    return $ applyOps emptyState $ intertwine winOps (a ++ b ++ c)

treeOps :: [SUNState -> SUNState]
treeOps = map (modify (tree . focusWS)) $
  replicate 4 (vSplit 0.5) ++
  replicate 4 (hSplit 0.5) ++
  replicate 10 (changeFocus U 1280 800) ++
  replicate 10 (changeFocus D 1280 800) ++
  replicate 10 (changeFocus L 1280 800) ++
  replicate 10 (changeFocus R 1280 800) ++
  replicate 7 (STree.resize R 0.02) ++
  replicate 7 (STree.resize D 0.02) ++
  replicate 7 (STree.resize L 0.02) ++
  replicate 7 (STree.resize U 0.02) ++
  replicate 2 flipTree ++
  replicate 2 makeEqual

wsOps :: [SUNState -> SUNState]
wsOps = map (modify focusWS) $
  replicate 10 (cycleHidden R) ++
  replicate 10 (cycleHidden L)

ssOps :: [SUNState -> SUNState]
ssOps = map moveToWS [1..9] ++ map changeWorkspace [1..9]

emptyState :: SUNState
emptyState = initState 9

intertwine :: [a] -> [a] -> [a]
intertwine a b = intertwine' a b []
intertwine' [] _ c = c
intertwine' _ [] c = c
intertwine' (a:as) (b:bs) c = intertwine' as bs (a:b:c)

applyOps :: SUNState -> [SUNState -> SUNState] -> SUNState
applyOps = foldl (flip modifySafe)

modifySafe :: (SUNState -> SUNState) -> SUNState -> SUNState
modifySafe sf ss
  | all notTooSmall $ map fst $ flatten (get (tree . focusWS) ss') 1280 800 = ss'
  | otherwise = ss
  where ss' = sf ss
        notTooSmall (_,_,w,h) = w > 50 && h > 50

prop_resizeUD :: SUNState -> Property
prop_resizeUD ss = property
    ((ssR == ss) || modify (tree . focusWS) (STree.resize D 0.02) ssR == ss)
  where ssR = modifySafe (modify (tree . focusWS) (STree.resize U 0.02)) ss

prop_resizeLR :: SUNState -> Property
prop_resizeLR ss = property
    ((ssR == ss) || modify (tree . focusWS) (STree.resize R 0.02) ssR == ss)
  where ssR = modifySafe (modify (tree . focusWS) (STree.resize L 0.02)) ss

prop_wsChange :: SUNState -> Property
prop_wsChange ss = property $ changeWorkspace fwsn ss' == updateWorkspace ss
  where fwsn = get focusWSNum ss
        ss' = applyOps ss (map changeWorkspace [1..9])

prop_moveWinAround :: SUNState -> Bool
prop_moveWinAround ss
  | isNothing (fromFrame $ get (tree . focusWS) ss) = True
  | otherwise =  all (== updateWorkspace ss) $ map trip [1..9]
  where trip nwsn = changeWorkspace fwsn $ moveToWS fwsn 
                    $ changeWorkspace nwsn $ moveToWS nwsn ss
        fwsn = get focusWSNum ss

prop_splitUnsplitV :: SUNState -> Bool
prop_splitUnsplitV ss
  | ss' == ss = True
  | otherwise = ss'' == ss
  where ss' = modifySafe (modify focusWS (doSplit (vSplit 0.5))) ss
        ss'' = modify focusWS killFrame 
               $ modify (tree . focusWS) (changeFocus R 1280 800) ss'

prop_splitUnsplitH :: SUNState -> Bool
prop_splitUnsplitH ss
  | ss' == ss = True
  | otherwise = ss'' == ss
  where ss' = modifySafe (modify focusWS (doSplit (hSplit 0.5))) ss
        ss'' = modify focusWS killFrame 
               $ modify (tree . focusWS) (changeFocus D 1280 800) ss'

prop_followTrailBack :: SUNState -> Bool
prop_followTrailBack ss = t == followTrailMap tMap (top t)
  where t = get (tree . focusWS) ss
        tMap = trailMap $ get trail t

prop_doubleFlip :: SUNState -> Bool
prop_doubleFlip ss = flipTree (flipTree t) == t
  where t = get (tree . focusWS) ss

prop_doubleCycleHidden :: SUNState -> Bool
prop_doubleCycleHidden ss 
  | isNothing (focusedWin $ get focusWS ss) = True
  | otherwise = cycleHidden R (cycleHidden L ws) == ws
             && (cycleHidden L (cycleHidden R ws) == ws)
  where ws = get focusWS ss

prop_deleteAllWins :: SUNState -> Bool
prop_deleteAllWins ss = null a
  where ss' = updateWorkspace ss
        aws = getAllWins ss'
        ss'' = applyOps ss' $ map annihilateWin aws
        a = getAllWins ss''
