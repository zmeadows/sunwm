{-# LANGUAGE BangPatterns, TemplateHaskell, TypeOperators #-}
module STree where

import FocusMap

import Prelude hiding ((.), id)
import Data.Label hiding (fw)
import Data.List (minimumBy, delete, find)
import Data.Maybe
import Data.Ord (comparing)

import Control.Category ((.))
--import Control.Arrow (first, second, (&&&))

import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Dimension, Position, Rectangle(..))

type SplitRatio = Double

data SplitType = H {_ratio :: SplitRatio} | V {_ratio :: SplitRatio}
    deriving (Read,Show,Eq)

data SUNTree = Frame (Maybe Window) | Split SplitType !SUNTree !(SUNTree)
    deriving (Read,Show,Eq)

data SUNPath = LU { _splitType :: SplitType, _splitTree :: !(SUNTree) }
             | RD { _splitType :: SplitType, _splitTree :: !(SUNTree) }
    deriving (Read,Show,Eq)

data SUNZipper = SZ { _focus :: !(SUNTree), _trail :: ![SUNPath] }
    deriving (Read,Show,Eq)

data Workspace = Workspace
    { _tree         :: !(SUNZipper)
    , _hidden       :: ![Window]
    , _inFullScreen :: !Bool
    } deriving (Read,Show,Eq)

data SUNScreen = SUNScreen
    { _workspaces   :: !(FocusMap Int Workspace)
    , _xPos         :: !Position
    , _yPos         :: !Position
    , _width        :: !Dimension
    , _height       :: !Dimension
    } deriving (Read,Show,Eq)

data SUNState = SUNState
    { _screens      :: !(FocusMap Int SUNScreen)
    , _inPrefix     :: !Bool
    , _barHeight    :: !Dimension
    } deriving (Read,Show,Eq)

data Direction = L | R | U | D
    deriving (Read,Show,Eq)

$(mkLabels [''SplitType, ''SUNPath, ''SUNZipper, ''Workspace, ''SUNScreen, ''SUNState])

-- | A frame with no window in it.
emptyFrame :: SUNTree
emptyFrame = Frame Nothing

-- | An empty zipper.  (Default layout upon starting sunwm)
emptyZipper :: SUNZipper
emptyZipper = SZ emptyFrame []

-- | An empty default workspace
emptyWS :: Workspace
emptyWS = Workspace emptyZipper [] False

emptyScr :: Int -> Rectangle -> SUNScreen
emptyScr !nws !(Rectangle x y w h) = SUNScreen wss x y w h
    where wss = fromList 1 $ zip [1..nws] $ replicate nws emptyWS

initState :: Int -> [Rectangle] -> SUNState
initState !nw !recs = SUNState scrs False 0
  where scrs = fromList 1 $ zip [1..length recs] $ map (emptyScr nw) recs

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

up :: SUNZipper -> SUNZipper
up sz@(SZ _ []) = sz
up (SZ t (LU st rd:ps)) = SZ (Split st t rd) ps
up (SZ t (RD st lu:ps)) = SZ (Split st lu t) ps

left :: SUNZipper -> SUNZipper
left (SZ (Split st lu rd) ps) = SZ lu (LU st rd:ps)
left sz = sz

right :: SUNZipper -> SUNZipper
right (SZ (Split st lu rd) ps) = SZ rd (RD st lu:ps)
right sz = sz

doSplit ::(SUNZipper -> SUNZipper) -> Workspace -> Workspace
doSplit splitFunc !ws = updateHidden $ modify tree splitFunc ws
  where hws = get hidden ws
        updateHidden ws'
          | null hws  = ws'
          | otherwise = modify tree
                (left . up . replace (Just $ head hws) . right . up)
                $ set hidden (tail hws) ws'

-- | Returns True if a Frame is focused, rather than a Split
isFrame :: SUNZipper -> Bool
isFrame !(SZ (Frame _) _) = True
isFrame !_ = False

-- | Return to the top of the SUNZipper
top :: SUNZipper -> SUNZipper
top !sz | null $ get trail sz = sz
        | otherwise = top $ up sz

-- | Move left/right down a tree
traverse :: (SUNZipper -> SUNZipper) -> SUNZipper -> SUNZipper
traverse leftOrRight !sz
    | isFrame sz = sz
    | otherwise = traverse leftOrRight $ leftOrRight sz

-- | Remove the currently focused frame, and if it was occupied add the
-- window the the hidden stack.
killFrame :: Workspace -> Workspace
killFrame !ws
  | null tr = ws
  | otherwise = hideWin $ modify tree deleteFrame ws
  where win = fromFrame $ get tree ws
        tr = get (trail . tree) ws
        hideWin = modify hidden (maybeToList win ++)
        deleteFrame !(SZ (Frame _) (LU _ rd:ps)) = traverse right (SZ rd ps)
        deleteFrame !(SZ (Frame _) (RD _ lu:ps)) = traverse left (SZ lu ps)
        deleteFrame !sz = sz

fromFrame :: SUNZipper -> Maybe Window
fromFrame (SZ (Frame a) _) = a
fromFrame _  = Nothing

isLU :: SUNPath -> Bool
isLU (LU _ _) = True
isLU _ = False
isRD :: SUNPath -> Bool
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
followTrailMap tm !sz = foldl (\sz' tf -> tf sz') sz tm

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

-- | 'flatten' is the core function of the window manager, calculating the size
-- of the windows themselves by recursively subdividing based on
-- splitRatios found in a SUNZipper
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

-- | Searches for the windows 'closest' to the specified direction
changeFocus :: Direction -> Dimension -> Dimension -> SUNZipper -> SUNZipper
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
      -- | TODO: can just use finder' only here and replace cands with ts'
      -- in above line?
  in if null cands then sz else newFocus

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

-- | TODO: condense 'resize' into one function
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
deleteWinTree :: Window -> SUNZipper -> SUNZipper
deleteWinTree w !sz = case delWinZip of
    Just dwz -> followTrailMap (trailMap $ get trail sz) $ top $ clear dwz
    Nothing -> sz
  where isDelWin (SZ (Frame a) _) = a == Just w
        isDelWin (SZ (Split {}) _) = False -- ERROR
        delWinZip = find isDelWin $ flattenToZips sz

-- | Removes a window from a whole workspace (even if it's hidden)
deleteWinWS :: Window -> Workspace -> Workspace
deleteWinWS w !ws = modify tree (deleteWinTree w) $ modify hidden (delete w) ws

deleteWinScr :: Window -> SUNScreen -> SUNScreen
deleteWinScr w = modify workspaces $ mapF $ deleteWinWS w

annihilateWin :: Window -> SUNState -> SUNState
annihilateWin w = modify screens $ mapF $ deleteWinScr w

-- | Switches view to another workspace.
changeWorkspace :: Int -> SUNScreen -> SUNScreen
changeWorkspace wsn = modify workspaces $ newFocus wsn

changeScreen :: Int -> SUNState -> SUNState
changeScreen scrn = modify screens $ newFocus scrn

-- | Brings a hidden window the the forefront and adds the currently
-- focused window to the hidden group.
cycleHidden :: Direction -> Workspace -> Workspace
cycleHidden dir !ws
    | null hw = ws
    | dir == R = set hidden (hs ++ w) newTreeR
    | dir == L = set hidden (w ++ hs') newTreeL
    | otherwise = ws -- can never happen...
  where hw = get hidden ws
        (h,hs,h',hs') = (head hw, tail hw, last hw, init hw)
        newTreeR = modify tree (replace (Just h)) ws
        newTreeL = modify tree (replace (Just h')) ws
        w = maybeToList $ fromFrame $ get tree ws

-- | Rotates the layout 90 degrees.
flipTree :: SUNZipper -> SUNZipper
flipTree !sz = let tsz = trailMap $ get trail sz
    in followTrailMap tsz $ set focus (flipIt (get focus $ top sz)) $ set trail [] sz
  where flipIt (Split (H ra) l r) = Split (V ra) (flipIt l) (flipIt r)
        flipIt (Split (V ra) l r) = Split (H ra) (flipIt l) (flipIt r)
        flipIt f@(Frame _) = f

-- | Sets all the split ratios in the tree to 0.5.
makeEqual :: SUNZipper -> SUNZipper
makeEqual !sz = let tsz = trailMap $ get trail sz
    in followTrailMap tsz $ set focus (equalIt (get focus $ top sz)) $ set trail [] sz
  where equalIt (Split (H _) l r) = Split (H 0.5) (equalIt l) (equalIt r)
        equalIt (Split (V _) l r) = Split (V 0.5) (equalIt l) (equalIt r)
        equalIt f@(Frame _) = f

getWSAllWins :: Workspace -> [Window]
getWSAllWins !ws =
    let avs = (flattenToWins . get tree) ws
        ahs = get hidden ws
    in avs ++ ahs

getScrAllWins :: SUNScreen -> [Window]
getScrAllWins = concatMap getWSAllWins . elems . get workspaces

-- | ------------- | --
-- | CUSTOM LENSES | --
-- | ------------- | --

screenN :: Int -> SUNState :-> SUNScreen
screenN n = lens getScreenN setScreenN
  where getScreenN !ss = get screens ss <!> n
        setScreenN !scr !ss = modify screens (updateK n scr) ss

focusScr :: SUNState :-> SUNScreen
focusScr = lens getFocusScr setFocusScr
  where getFocusScr = focused . get screens
        setFocusScr !scr !ss = modify screens (update scr) ss

workspaceN :: Int -> SUNState :-> Workspace
workspaceN n = lens getWorkspaceN setWorkspaceN
  where getWorkspaceN !ss = get (workspaces . focusScr) ss <!> n
        setWorkspaceN !ws = modify (workspaces . focusScr) (updateK n ws)

focusWS :: SUNState :-> Workspace
focusWS = lens getFocusWS setFocusWS
  where getFocusWS = focused . get (workspaces . focusScr)
        setFocusWS !ws = modify (workspaces . focusScr) (update ws)

-- | Moves the currently focused window to another workspace
moveToWS :: Int -> FocusMap Int Workspace -> FocusMap Int Workspace
moveToWS nwsn m@(fwsn,_)
    | nwsn == fwsn || isNothing fw = m
    | otherwise = adjustK nwsn (modify tree $ replace fw)
                  $ adjustK nwsn (modify hidden (maybeToList sfw ++)) m'
  where fw = fromFrame $ get tree $ focused m
        m' = adjustK fwsn (cycleHidden R . modify tree clear) m
        sfw = fromFrame $ get tree $ m <!> nwsn

{-
moveToScr :: Int -> FocusMap Int SUNScreen -> FocusMap Int SUNScreen
moveToScr nscn m@(fscn,scs)
    | nscn == fscn ||
  where fw =



getWorkspaces = M.elems . snd . get workspaces

setWS :: Int -> Workspace -> SUNScreen -> SUNScreen
setWS n nws = modifyWS n (\_ -> nws)

modifyFocWS :: (Workspace -> Workspace) -> SUNScreen -> SUNScreen
modifyFocWS f !ss = modifyWS (getFocusWSNum ss) f ss

modifyTree :: Int -> (SUNZipper -> SUNZipper) -> SUNScreen -> SUNScreen
modifyTree n f = modifyWS n (modify tree f)

modifyFocTree :: (SUNZipper -> SUNZipper) -> SUNScreen -> SUNScreen
modifyFocTree f = modifyFocWS (modify tree f)

modifyHidden :: Int -> ([Window] -> [Window]) -> SUNScreen -> SUNScreen
modifyHidden n f = modifyWS n (modify hidden f)

modifyFocHidden :: ([Window] -> [Window]) -> SUNScreen -> SUNScreen
modifyFocHidden f = modifyFocWS (modify hidden f)

getWorkspaces :: SUNScreen -> [Workspace]
getWorkspaces = M.elems . snd . get workspaces

getTree :: Int -> SUNScreen -> SUNZipper
getTree n = get tree . getWS n

getFocusTree :: SUNScreen -> SUNZipper
getFocusTree !ss = getTree (getFocusWSNum ss) ss

getFocusTrail = get trail . getFocusTree


getScr :: Int -> SUNState -> SUNScreen
getScr n = fromJust . M.lookup n . snd . get screens

getFocusScrNum :: SUNState -> Int
getFocusScrNum = fst . get screens

getFocusScr :: SUNState -> SUNScreen
getFocusScr !st = getScr (getFocusScrNum st) st


-}