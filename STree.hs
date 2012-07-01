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

{-# LANGUAGE BangPatterns, TemplateHaskell, TypeOperators #-}
module STree where

import Control.Category
import Prelude hiding ((.), id)
import Data.Label hiding (fw)

import Data.List (minimumBy, delete, find)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Ord (comparing)

import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Dimension, Position)

type SplitRatio = Double

data SplitType = H {_ratio :: SplitRatio} | V {_ratio :: SplitRatio}
    deriving (Show, Eq)

data SUNTree = Frame (Maybe Window)
             | Split SplitType !SUNTree !(SUNTree)
    deriving (Show, Eq)

data SUNPath = LU { _splitType :: SplitType, _splitTree :: !(SUNTree) }
             | RD { _splitType :: SplitType, _splitTree :: !(SUNTree) }
    deriving (Show, Eq)

data SUNZipper = SZ { _focus :: !(SUNTree), _trail :: ![SUNPath] }
    deriving (Show, Eq)

data SUNState = SUNState
    { _focusWS      :: !(WorkSpace)
    , _focusWSNum   :: Int
    , _workspaces   :: ![WorkSpace]
    , _inPrefix     :: Bool
    , _dragging     :: !(Maybe DragType)
    , _barHeight    :: !Dimension
    , _screenWidth  :: !Dimension
    , _screenHeight :: !Dimension
    , _inFullScreen :: Bool
    } deriving (Show, Eq)

data WorkSpace = WorkSpace
    { _tree       :: !(SUNZipper)
    , _hidden     :: ![Window]
    , _floats     :: ![Window]
    , _focusFloat :: !(Maybe Window)
    } deriving (Show, Eq)

data Direction = L | R | U | D
    deriving (Show, Eq)

data DragType = Resize Window Dimension Dimension 
              | Move Window Position Position Position Position
    deriving (Show, Eq)

-- Generate Lenses for all data types using fclabels.
$(mkLabels [''SplitType, ''SUNPath, ''SUNZipper, ''SUNState, ''WorkSpace])

-- | A frame with no window in it.
emptyFrame :: SUNTree
emptyFrame = Frame Nothing

-- | An empty zipper.  (Default layout upon starting sunwm)
emptyZipper :: SUNZipper
emptyZipper = SZ emptyFrame []

emptyWS :: WorkSpace
emptyWS = WorkSpace emptyZipper [] [] Nothing

walkTrail :: SUNPath -> SUNZipper -> SUNZipper
walkTrail p = modify trail (p:)

replace :: Maybe Window -> SUNZipper -> SUNZipper
replace mw = set focus (Frame mw)

parent :: SUNZipper -> SUNPath
parent = head . get trail

clear :: SUNZipper -> SUNZipper
clear = replace Nothing

vSplit :: SplitRatio -> SUNZipper -> SUNZipper
vSplit r = walkTrail (LU (V r) emptyFrame)

hSplit :: SplitRatio -> SUNZipper -> SUNZipper
hSplit r = walkTrail (LU (H r) emptyFrame)

doSplit ::  (SUNZipper -> SUNZipper) -> WorkSpace -> WorkSpace
doSplit splitFunc !ws = updateHidden $ modify tree splitFunc ws
  where hws = get hidden ws
        updateHidden ws'
          | not $ null hws = modify tree 
                (left . up . replace (Just $ head hws) . right . up)
                $ set hidden (tail hws) ws'
          | otherwise = ws'

isFrame :: SUNZipper -> Bool
isFrame !(SZ (Frame _) _) = True
isFrame !_ = False

up :: SUNZipper -> SUNZipper
up sz@(SZ _ []) = sz
up (SZ t (LU st rd:ps)) = SZ (Split st t rd) ps
up (SZ t (RD st lu:ps)) = SZ (Split st lu t) ps

top :: SUNZipper -> SUNZipper
top !sz | null $ get trail sz = sz
        | otherwise = top $ up sz

traverse :: (SUNZipper -> SUNZipper) -> SUNZipper -> SUNZipper
traverse leftOrRight !sz
    | isFrame sz = sz
    | otherwise = traverse leftOrRight $ leftOrRight sz

-- | Remove the currently focused frame, and if it was occupied add the
-- window the the hidden stack.
killFrame :: WorkSpace -> WorkSpace
killFrame !ws
  | null tr = ws
  | otherwise = hideWin $ modify tree deleteFrame ws
  where win = focusedWin ws
        tr = get (trail . tree) ws
        hideWin = if isJust win then modify hidden (fromJust win :) else id
        deleteFrame !(SZ (Frame _) (LU _ rd:ps)) = traverse right (SZ rd ps)
        deleteFrame !(SZ (Frame _) (RD _ lu:ps)) = traverse left (SZ lu ps)
        deleteFrame !sz = sz

left :: SUNZipper -> SUNZipper
left (SZ (Split st lu rd) ps) = SZ lu (LU st rd:ps)
left sz = sz

right :: SUNZipper -> SUNZipper
right (SZ (Split st lu rd) ps) = SZ rd (RD st lu:ps)
right sz = sz

isLU, isRD :: SUNPath -> Bool
isLU (LU _ _)= True
isLU _ = False
isRD (RD _ _) = True
isRD _ = False

isSplit :: SUNZipper -> Bool
isSplit !(SZ (Split {}) _) = True
isSplit !_ = False

-- | Creates a list of functions, which if applied to a SUNZipper brought
-- to it's top level (using followTrailMap), return it to a unique state.
trailMap :: [SUNPath] -> [SUNZipper -> SUNZipper]
trailMap !tr = trailMap' tr []
  where 
   trailMap' [] tm = tm
   trailMap' (t:tr') tm
     | isLU t = trailMap' tr' (left:tm)
     | isRD t = trailMap' tr' (right:tm)
   trailMap' (_:_) tm = tm -- add error handling here

followTrailMap :: [SUNZipper -> SUNZipper] -> SUNZipper -> SUNZipper
followTrailMap tm !sz = Prelude.foldl (\sz' tf -> tf sz') sz tm

flatten :: (Integral t1, Integral t) => SUNZipper -> t -> t1 -> [((t, t1, t, t1), SUNZipper)]
flatten !szip wth hgt = flatten' (top szip) (0,0,wth,hgt)
  where flatten' sz@(SZ (Frame _) _) dims = [(dims,sz)]
        flatten' sz@(SZ (Split (V r) _ _) _) (x,y,w,h) =
          let  luDim = (x,y,r.*.w,h)
               rdDim = (x+(r.*.w),y,w - (r.*.w),h)
          in flatten' (left sz) luDim ++ flatten' (right sz) rdDim
        flatten' sz@(SZ (Split (H r) _ _) _) (x,y,w,h) =
          let luDim = (x,y,w,r.*.h)
              rdDim = (x,y+(r.*.h),w,h-(r.*.h))
          in flatten' (left sz) luDim ++ flatten' (right sz) rdDim

(.*.) :: Integral a => SplitRatio -> a -> a
(.*.) a b = round $ a * fi b

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fir :: Integral a => a -> SplitRatio
fir x = fromIntegral x :: SplitRatio

fip :: Integral a => a -> Position
fip x = fromIntegral x :: Position

fid :: Integral a => a -> Dimension
fid x = fromIntegral x :: Dimension

flattenToDimWins :: Dimension -> Dimension -> SUNZipper 
                    -> [((Position, Position, Dimension, Dimension), Window)]
flattenToDimWins sw sh !sz = map pullWin $ filter isWin $ flatten sz sw sh
    where isWin (_,t) = (/= Frame Nothing) $ get focus t
          pullWin (d,SZ (Frame (Just a)) _) = (convert d,a)
          pullWin (d,SZ (Frame Nothing) _) = (convert d, 0) -- ERROR
          pullWin (d,SZ (Split {}) _) = (convert d,0) -- ERROR
          convert (x,y,w,h) = (fip x, fip y, fid w, fid h)

flattenToWins ::  SUNZipper -> [Window]
flattenToWins !sz = flattenToWins' $ top sz
  where flattenToWins' (SZ (Frame Nothing) _) = []
        flattenToWins' (SZ (Frame (Just w)) _) = [w]
        flattenToWins' !sz' = flattenToWins' (left sz') ++ flattenToWins' (right sz')

flattenToZips ::  SUNZipper -> [SUNZipper]
flattenToZips !sz = flattenToZips' $ top sz
  where flattenToZips' (SZ (Frame Nothing) _) = []
        flattenToZips' sz'@(SZ (Frame (Just _)) _) = [sz']
        flattenToZips' !sz' = flattenToZips' (left sz') ++ flattenToZips' (right sz')

changeFocus :: Integral n => Direction -> n -> n -> SUNZipper -> SUNZipper
changeFocus dir scrw scrh !sz =
  let ts = flatten sz scrw scrh
      (Just curW@((x,y,w,h),_)) = find ((==) sz . snd) ts
      ts' = Data.List.delete curW ts
      finder ((x',y',w',h'),_) = case dir of
        L -> abs (x' + w' - x) < 10
        R -> abs (x + w - x') < 10
        U -> abs (y' + h' - y) < 10
        D -> abs (y + h - y') < 10
      cands = filter finder ts'
      finder' (x',y',w',h') = case dir of
        L -> min (abs ((y + h)-(y' + h'))) (abs (y - y'))
        R -> min (abs ((y + h)-(y' + h'))) (abs (y - y'))
        U -> min (abs ((x + h)-(x' + w'))) (abs (x - x'))
        D -> min (abs ((x + h)-(x' + w'))) (abs (x - x'))
      newFocus = snd $ minimumBy (comparing (finder' . fst)) cands
  in case cands of
        [] -> sz
        _  -> newFocus

fromFrame :: SUNZipper -> Maybe Window
fromFrame (SZ (Frame a) _) = a
fromFrame _  = Nothing

isVSplit :: SUNPath -> Bool
isVSplit (LU (V _) _) = True
isVSplit (RD (V _) _) = True
isVSplit _ = False

isHSplit :: SUNPath -> Bool
isHSplit (LU (H _) _) = True
isHSplit (RD (H _) _) = True
isHSplit _ = False

(<->) :: SUNPath -> SplitRatio -> SUNPath
(<->) path dr = modify (ratio . splitType) (+ dr) path

resizeParent :: SplitRatio -> (SUNPath -> Bool) -> [SUNPath] -> [SUNPath]
resizeParent dr pathTypeChecker paths
  | psl == paths || psr == [] = paths
  | otherwise = psl ++ (head psr <-> dr : tail psr)
  where (psl,psr) = break pathTypeChecker paths

resize :: Direction -> SplitRatio -> SUNZipper -> SUNZipper
resize R dr !sz
  | get trail sz == [] = sz
  | isHSplit p = set trail (resizeParent dr isVSplit pss) sz
  | isVSplit p = set trail (p <-> dr:ps) sz
  | otherwise = sz
 where pss@(p:ps) = get trail sz
resize L dr !sz
  | get trail sz == [] = sz
  | isHSplit p = set trail (resizeParent (-dr) isVSplit pss) sz
  | isVSplit p = set trail (p <-> (-dr):ps) sz
  | otherwise = sz
 where pss@(p:ps) = get trail sz
resize U dr !sz
  | get trail sz == [] = sz
  | isVSplit p = set trail (resizeParent (-dr) isHSplit pss) sz
  | isHSplit p = set trail (p <-> (-dr):ps) sz
  | otherwise = sz
 where pss@(p:ps) = get trail sz
resize D dr !sz
  | get trail sz == [] = sz
  | isVSplit p = set trail (resizeParent dr isHSplit pss) sz
  | isHSplit p = set trail (p <-> dr:ps) sz
  | otherwise = sz
 where pss@(p:ps) = get trail sz

-- | Removes a (currently visible) window (focused or not) from SUNZipper, assuming it is present.
deleteWin :: Window -> SUNZipper -> SUNZipper
deleteWin w !sz = case delWinZip of
    Just dwz -> followTrailMap (trailMap $ get trail sz) $ top $ clear dwz
    Nothing -> sz
  where isDelWin (SZ (Frame a) _) = a == Just w
        isDelWin (SZ (Split {}) _) = False -- ERROR
        delWinZip = find isDelWin $ flattenToZips sz

-- | Removes a window from a whole workspace (even if it's hidden)
destroyWin :: Window -> WorkSpace -> WorkSpace
destroyWin w !ws = modify tree (deleteWin w) $ modify hidden (Data.List.delete w) ws

-- | Removes a window from any workspace it may be on, hidden or not.
annihilateWin :: Window -> SUNState -> SUNState
annihilateWin w !ss
  | w `elem` getAllWins ss = ss'
  | otherwise = ss
 where ss' = modify focusWS (destroyWin w) 
           $ modify workspaces (map (destroyWin w)) 
           $ updateWorkspace ss

getAllWins :: SUNState -> [Window]
getAllWins !ss = concatMap (\ws -> getHidWins ws ++ getVisWins ws) 
                 $ get workspaces $ updateWorkspace ss

getVisWins :: WorkSpace -> [Window]
getVisWins ws = flattenToWins (get tree ws)

getHidWins :: WorkSpace -> [Window]
getHidWins = get hidden 

getAllHiddenWins :: SUNState -> [Window]
getAllHiddenWins !ss = concatMap (get hidden) $ get workspaces $ updateWorkspace ss

-- | Sets all the split ratios in the tree to 0.5.
makeEqual :: SUNZipper -> SUNZipper
makeEqual !sz = let tsz = trailMap $ get trail sz
    in followTrailMap tsz $ set focus (equalIt (get focus $ top sz)) $ set trail [] sz
  where equalIt (Split (H _) l r) = Split (H 0.5) (equalIt l) (equalIt r)
        equalIt (Split (V _) l r) = Split (V 0.5) (equalIt l) (equalIt r)
        equalIt f@(Frame _) = f

-- | Rotates the layout 90 degrees.
flipTree :: SUNZipper -> SUNZipper
flipTree !sz = let tsz = trailMap $ get trail sz
    in followTrailMap tsz $ set focus (flipIt (get focus $ top sz)) $ set trail [] sz
  where flipIt (Split (H ra) l r) = Split (V ra) (flipIt l) (flipIt r)
        flipIt (Split (V ra) l r) = Split (H ra) (flipIt l) (flipIt r)
        flipIt f@(Frame _) = f

initState :: Int -> SUNState
initState !nw = SUNState w 1 ws False Nothing 0 0 0 False
    where ws@(w:_) = replicate nw emptyWS

focusedWin :: WorkSpace -> Maybe Window
focusedWin = fromFrame . get tree

-- | Brings a hidden window the the forefront and adds the currently
-- focused window to the hidden group.
cycleHidden :: Direction -> WorkSpace -> WorkSpace
cycleHidden dir ws
    | null hw = ws
    | dir == R = set hidden (hs ++ w) newTreeR
    | dir == L = set hidden (w ++ hs') newTreeL
    | otherwise = ws
  where hw = get hidden ws
        (h,hs,h',hs') = (head hw, tail hw, last hw, init hw)
        newTreeR = modify tree (replace (Just h)) ws
        newTreeL = modify tree (replace (Just h')) ws
        w = let fw = focusedWin ws in [fromJust fw | isJust fw]

-- | A convenience function for retrieving common state values.
sunGet ::  SUNState -> (Maybe Window, WorkSpace, Int, [WorkSpace])
sunGet ss = (fw,fws,fwsn,wss)
  where fws  = get focusWS ss ; fwsn = get focusWSNum ss
        wss  = get workspaces ss ; fw = fromFrame $ get (tree . focusWS) ss

-- | Ensures that the representation of the focused workspace in the
-- workspaces list is the same as the current state of the focused workspace.
updateWorkspace :: SUNState -> SUNState
updateWorkspace ss = set workspaces (init ls ++ (fws:rs)) ss 
  where (_,fws,fwsn,wss)  = sunGet ss
        (ls,rs) = splitAt fwsn wss

-- | Moves the currently focused window to another workspace
moveToWS :: Int -> SUNState -> SUNState
moveToWS wsn ss
    | isJust ffw = 
        let ffw' = fromJust ffw
            nws  = modify floats (ffw':) $ set focusFloat (Just ffw') sws
            in set workspaces (init ls ++ nws:rs) 
             $ set (focusFloat . focusWS) Nothing 
             $ modify (floats . focusWS) (Data.List.delete ffw') ss
    | fwsn == wsn || isNothing fw = ss
    | isJust sfw = 
        let nws = modify tree (replace fw) $ modify hidden (fromJust sfw:) sws
        in set workspaces (init ls ++ nws:rs) ss'
    | otherwise = 
        let nws = modify tree (replace fw) sws
        in set workspaces (init ls ++ nws:rs) ss'
  where (fw,_,fwsn,_) = sunGet ss
        ss' = modify focusWS (cycleHidden R) 
              $ modify (tree . focusWS) clear ss
        sws = get workspaces ss' !! (wsn-1)
        sfw = fromFrame $ get tree sws
        ffw = get (focusFloat . focusWS) ss
        (ls,rs) = splitAt wsn $ get workspaces ss'

-- | Switches view to another workspace.
changeWorkspace :: Int -> SUNState -> SUNState
changeWorkspace wsn ss = set focusWSNum wsn $ set focusWS nws ss'
  where ss' = updateWorkspace ss
        (_,_,_,wss) = sunGet ss'
        nws = wss !! (wsn-1)
