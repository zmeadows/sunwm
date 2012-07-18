{- Copyright 2011-2012 Zac Meadows

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

-- --------------------------------------------------------------------------
-- Module      :  SunWM.Core
-- Copyright   :  (c) Zac Meadows 2012
-- License     :  GPL3 (see LICENSE)
--
-- Maintainer  :  zmeadows@gmail.com
--
-- Core interaction between Xlib and the data structures/functions defined
-- in STree.hs.
-- --------------------------------------------------------------------------
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}
module Core where

import STree
import Util

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CLong)

import Control.Monad.State hiding (gets)
import Control.Monad.Reader hiding (asks)
import Control.Monad.Error
import Control.Category
import Control.Concurrent
import Control.Applicative

import Data.Bits
import Data.List (delete, (\\), find, intercalate)
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Data.Label.PureM as P
import Data.Label as L hiding (fw,bw)
import qualified Data.Map as M (Map, lookup, keys)

import System.IO
import System.Exit

import Prelude hiding ((.), id)

newtype SUN a = SUN (ErrorT String (ReaderT SUNConf (StateT SUNState IO)) a)
    deriving (Monad, MonadPlus, MonadIO, MonadState SUNState, MonadReader SUNConf, Functor)

data SUNConf = SUNConf
    { _display            :: Display
    , _root               :: !Window
    , _userConf           :: !UserConf
    , _normalBorderColor  :: !Pixel
    , _focusedBorderColor :: !Pixel
    , _backgroundColor    :: !Pixel
    , _foregroundColor    :: !Pixel
    , _numlockMask        :: !KeyMask
    }

data UserConf = UserConf
    { _normalBorder  :: !String
    , _focusedBorder :: !String
    , _borderWidth   :: !Dimension
    , _keyBinds      :: !(M.Map (KeyMask, KeySym) (SUN ()))
    , _topKeyBinds   :: !(M.Map (KeyMask, KeySym) (SUN ()))
    , _wsNames       :: ![String]
    , _prefixKey     :: !(KeyMask, KeySym)
    , _barConf       :: !XMobarConf
    , _terminal      :: !String
    , _maxUndo       :: Int
    , _font          :: !String
    , _background    :: !String
    , _foreground    :: !String
    }

type XMColor = (String, String)

data XMobarConf = XMobarConf
    { _currentC     :: !XMColor
    , _hiddenC      :: !XMColor
    , _hiddenEmptyC :: !XMColor
    , _titleC       :: !XMColor
    , _handle       :: !Handle 
    } deriving (Show, Eq)

$(mkLabels [''SUNConf, ''UserConf, ''XMobarConf])

-- | Configure the sizes of all mapped windows according to current tree shape. 
arrange :: SUN ()
arrange = asks display >>= \dis -> do
    t <- gets (tree . focusWS)
    (sw,sh) <- getScrDims
    bw <- (2 *) <$> asks (borderWidth . userConf)
    bh <- fip <$> gets barHeight
    let ws = flattenToDimWins sw sh t
        putWindow ((x,y,w,h),win) = moveResizeWindow dis win x (y + bh) (w - bw) (h - bw)
    when (length ws > 1 || L.get trail t /= [])
      $ liftIO $ mapM_ putWindow ws
    when (length ws == 1 && L.get trail t == [])
      $ liftIO $ moveResizeWindow dis (snd $ head ws) 0 bh sw sh

-- | Common non-predefined atoms
atomWMDELETEWINDOW, atomWMPROTOCOLS, atomWMSTATE :: SUN Atom
atomWMDELETEWINDOW = getAtom "WM_DELETE_WINDOW"
atomWMPROTOCOLS    = getAtom "WM_PROTOCOLS"
atomWMSTATE        = getAtom "WM_STATE"

-- | Move mouse to far bottom right corner of screen.
banish :: SUN ()
banish = do
    dis <- asks display ; rt <- asks root ; bh <- fip <$> gets barHeight
    (sw,sh) <- getScrDims
    liftIO $ warpPointer dis none rt 0 0 0 0 (fi sw) $ fi sh + bh

-- | Switch to another workspace
changeWS :: Int -> SUN ()
changeWS wsn = gets focusWSNum >>= \fwsn -> do
    when (fwsn /= wsn) $ do
      modifyState (changeWorkspace wsn)
      lastWS =: fwsn
    refresh >> arrange >> updateBar >> updateFocus

-- | Strip numlock && capslock from a mask
cleanMask :: KeyMask -> SUN KeyMask
cleanMask km = asks numlockMask >>= \nml ->
  return (complement (nml .|. lockMask) .&. km)

-- | Click-to-focus floating or tiled windows
clickFocus :: Window -> SUN ()
clickFocus w = do
  tr <- gets (tree . focusWS)
  fs <- gets (floats . focusWS)
  vws <- getVisWins <$> gets focusWS
  when (w `elem` fs) $ (focusFloat . focusWS) =: Just w
  when (w `elem` vws) $ do
    let zs = filter ((/=) Nothing . fromFrame) $ flattenToZips tr
        (Just cwz) = find ((==) (Just w) . fromFrame) zs
    (tree . focusWS) =: cwz
    (focusFloat . focusWS) =: Nothing
  updateFocus >> updateBar

-- | Takes care of click-to-focus for empty frames
clickFocusEmptyFrame :: (Dimension, Dimension) -> SUN ()
clickFocusEmptyFrame (x,y) = do
  sw <- gets screenWidth ; sh <- gets screenHeight
  t <- gets (tree . focusWS)
  let zs = filter ((==) Nothing . fromFrame . snd) $ flatten t sw sh
      cwz = find (isInRectangle (x,y) . fst) zs
  when (isJust cwz) $ (tree . focusWS) =: snd (fromJust cwz)
  arrange >> refresh >> updateFocus >> updateBar
 where isInRectangle (x',y') (rx,ry,rw,rh) =
        (x' > rx) && (x' < (rx+rw)) && (y' > ry) && (y' < (ry + rh))

colon :: SUN ()
colon = return ()

-- | Detects bar/screen dimensions and sets state values accordingly
-- Continues to attempt to detect bar for 10 seconds until it gives up
-- to account for launch time of xmobar.
-- TODO: Find a non-hackish way to do this.
configureBarScr :: Int -> SUN ()
configureBarScr n = do
  dis <- asks display ; rt <- asks root
  (_,_,qt) <- liftIO $ queryTree dis rt
  qt' <- filterM isDock qt
  let scr = defaultScreen dis
      sw = fid (displayWidth dis scr)
      sh = fid (displayHeight dis scr)
  screenWidth =: sw
  unless (null qt') $ do
      bwa <- liftIO $ getWindowAttributes dis (head qt')
      let bh = fid $ wa_height bwa + wa_y bwa
      barHeight =: bh
      screenHeight =: sh - bh
  when (null qt' && n < 100) $ do
      liftIO $ threadDelay 100000
      configureBarScr (n+1)
  when (null qt' && n >= 100) $ do
      barHeight =: 0
      screenHeight =: sh

-- | dmenu spawner
dmenu :: MonadIO m => String -> String -> String -> String -> String -> m ()
dmenu fn nb nf sb sf = spawn $ concat
      [ "dmenu_run "
      , "-nb '" ++ nb ++ "' "
      , "-nb '" ++ nb ++ "' "
      , "-sb '" ++ sb ++ "' "
      , "-sf '" ++ sf ++ "' "
      , "-nf '" ++ nf ++ "' "
      , "-fn '" ++ fn ++ "' "]

-- | Draws the border around a focused, empty frame.
drawFrameBorder :: SUN ()
drawFrameBorder = do
    (dis,rt,sw,sh,_,fc,uc) <- getConf
    t <- gets (tree . focusWS)
    fs <- gets (floats . focusWS)
    bh <- gets barHeight
    let mt = find ((==) t . snd) $ flatten t sw sh
        bw = L.get borderWidth uc
    when (isJust mt && null fs) $ liftIO $ do
        let ((x,y,w,h),_) = fromJust mt
        gc <- createGC dis rt
        setLineAttributes dis gc (fi bw) lineDoubleDash capButt joinMiter
        setForeground dis gc fc
        drawRectangle dis rt gc (fi x + div (fip bw) 2) (fi y + div (fip bw) 2 + fip bh) (w - fid bw) (h - fid bw)
        sync dis False
        freeGC dis gc

-- | Make all split ratios in the tree 0.5
equalize :: SUN ()
equalize = do
  storeUndo
  safeModify (tree . focusWS) makeEqual
  arrange >> refresh >> updateBar >> updateFocus

-- | Dispatch X11 events based on their type
eventDispatch :: Event -> SUN ()

eventDispatch !(UnmapEvent {ev_window = w, ev_send_event = synthetic}) = when synthetic (removeWindow w)

eventDispatch !(MapRequestEvent {ev_window = win}) = do
    dis <- asks display
    fw <- focusedWin <$> gets focusWS
    allWins <- stateFunc getAllWins
    isF <- isFullscreen win ; isDia <- isDialog win ; isS <- isSplash win
    isDk <- isDock win
    unless (win `elem` allWins || isF || isDia || isS || isDk) $ do
      when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
      (tree . focusWS) =. replace (Just win)
      (focusFloat . focusWS) =: Nothing
      arrange >> refresh >> updateFocus >> updateBar
    when (isF || isS || isDia) $ float win
    when isDk $ liftIO $ mapWindow dis win

eventDispatch !evt@(ButtonEvent {ev_window = w, ev_event_type = t, ev_x = x, ev_y = y})
  | t == buttonPress = do
      rt <- asks root
      when (w == rt) $ clickFocusEmptyFrame (fid x, fid y)
      when (w /= rt) $ do
      cm <- cleanMask $ ev_state evt
      when ((cm, ev_button evt) == (0,button1)) $ clickFocus w
      when ((cm, ev_button evt) == (mod1Mask,button1)) $ mouseMove w
      when ((cm, ev_button evt) == (mod1Mask,button3)) $ mouseResize w

eventDispatch !(MotionEvent {ev_event_type = _t, ev_x = x, ev_y = y}) = do
  drag <- gets dragging
  dis <- asks display
  case drag of
    (Just (Move win wax way pox poy)) ->
      liftIO $ moveWindow dis win (wax + (fip x - pox)) (way + (fip y - poy))
    (Just (Resize win w h)) ->
      liftIO $ resizeWindow dis win (fid x - w) (fid y - h)

    Nothing -> return () -- This shouldn't ever happen though. 

eventDispatch !(ButtonEvent {ev_event_type = t})
  | t == buttonRelease = do
      drag <- gets dragging
      case drag of
        (Just _) -> do
          dis <- asks display
          liftIO $ ungrabPointer dis currentTime
          dragging =: Nothing
        Nothing -> return ()

eventDispatch !(DestroyWindowEvent {ev_window = w}) = removeWindow w

eventDispatch !evt@(ConfigureRequestEvent _ _ _ dis _ win x y w h bw a d vm) = do
    ws <- getVisWins <$> gets focusWS
    wa <- liftIO $ getWindowAttributes dis win
    if win `notElem` ws
        then liftIO $ configureWindow dis win vm $ WindowChanges x y w h bw a d
        else liftIO $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev win win
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width evt) none 
                     (wa_override_redirect wa)
                 sendEvent dis win False 0 ev
    liftIO $ sync dis False
    arrange >> updateFocus >> updateBar

eventDispatch !evt@(KeyEvent {ev_event_type = et}) = when (et == keyPress) $ do
    dis <- asks display ; rt <- asks root ; inP <- gets inPrefix ; p <- asks (prefixKey . userConf)
    let km = ev_state evt ; kc = ev_keycode evt ; makeCursor c = liftIO $ createFontCursor dis c
    ks <- liftIO $ keycodeToKeysym dis kc 0
    when ((km,ks) == p && not inP) $ do
      inPrefix =: True
      cur <- makeCursor xC_rtl_logo
      _ <- liftIO $ grabPointer dis rt False 0 grabModeAsync grabModeAsync none cur currentTime
      _ <- liftIO $ grabKeyboard dis rt True grabModeAsync grabModeAsync currentTime
      liftIO $ freeCursor dis cur
    when inP $ do
      kbs <- asks (keyBinds . userConf)
      let kt = M.lookup (km,ks) kbs
      case kt of
          (Just act) -> act >> do
            liftIO $ ungrabKeyboard dis currentTime
            liftIO $ ungrabPointer dis currentTime
            inPrefix =: False
            grabPrefixTops
          Nothing -> return ()
    when ((km,ks) /= p && not inP) $ do
      tkbs <- asks (topKeyBinds . userConf)
      let kt = M.lookup (km,ks) tkbs
      case kt of
          (Just k) -> k
          Nothing -> return ()

eventDispatch !_ = return ()

-- | Core function of the whole window manager.  Receives the events
-- and sends them to eventDispatch.
eventLoop :: SUN ()
eventLoop = asks display >>= \dis ->
  forever (liftIO $ do
    sync dis False
    allocaXEvent $ \e -> nextEvent dis e >> getEvent e)
      >>= eventDispatch

-- | Rotate current layout by 90 degrees
flipT :: SUN ()
flipT = do
  storeUndo 
  safeModify (tree . focusWS) flipTree
  arrange >> refresh >> updateBar >> updateFocus

-- | Float the currently focused window
float :: Window -> SUN ()
float w = do
  modifyState (annihilateWin w)
  (focusFloat . focusWS) =: Just w
  fs <- gets (floats . focusWS) 
  unless (w `elem` fs) $ do
    (floats . focusWS) =. (w:)
    dialog <- isDialog w
    unless dialog $ focusWS =. cycleHidden R
  arrange >> refresh >> updateFocus >> updateBar

focusTo :: Direction -> SUN ()
focusTo dir = do
  ffw <- gets (focusFloat . focusWS)
  when (isNothing ffw) $ do
    sw <- gets screenWidth ; sh <- gets screenHeight
    safeModify (tree . focusWS) $ changeFocus dir sw sh
    arrange >> refresh >> updateFocus >> updateBar

-- | Wrapper for the common case of atom internment
getAtom :: String -> SUN Atom
getAtom str = asks display >>= \dis -> liftIO $ internAtom dis str False

getColor :: Display -> ScreenNumber -> String -> IO Pixel
getColor dis scr str = let cMap = defaultColormap dis scr in
    (color_pixel . fst) <$> allocNamedColor dis cMap str

getConf :: SUN (Display, Window, Dimension, Dimension, Pixel, Pixel, UserConf)
getConf = do
    dis <- asks display ; fc <- asks focusedBorderColor ; uc <- asks userConf
    nc <- asks normalBorderColor ; sw <- gets screenWidth
    sh <- gets screenHeight ; rt <- asks root
    return (dis,rt,sw,sh,nc,fc,uc)

-- | Get a window property from atom
getProp32 :: Atom -> Window -> SUN (Maybe [CLong])
getProp32 a w = asks display >>= \dis -> liftIO $ getWindowProperty32 dis a w

-- | Get a window property from string
getProp32s :: String -> Window -> SUN (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }

-- | Return the screen dimensions (width, height)
getScrDims :: SUN (Dimension, Dimension)
getScrDims = do
    sw <- gets screenWidth
    sh <- gets screenHeight
    return (sw,sh)

-- | Grab prefix key and top-level bindings.
grabPrefixTops :: SUN ()
grabPrefixTops = do
    dis <- asks display
    rt <- asks root
    keys <- M.keys <$> asks (topKeyBinds . userConf)
    liftIO $ ungrabKey dis anyKey anyModifier rt
    let xGrabKey (km,kc) = grabKey dis kc km rt True grabModeSync grabModeAsync
        toKeyCode (km,ks) = do
            kc <- keysymToKeycode dis ks
            return (km,kc)
    keys' <- liftIO $ mapM toKeyCode keys
    p <- asks (prefixKey . userConf)
    p' <- liftIO $ toKeyCode p
    liftIO $ xGrabKey p' >> mapM_ xGrabKey keys'

-- | Window property analysis functions
isInProperty :: String -> String -> Window -> SUN Bool
isInProperty p v w = do
  va <- getAtom v
  r <- getProp32s p w
  return $ case r of
    Just xs -> fromIntegral va `elem` xs
    _ -> False

isDialog :: Window -> SUN Bool
isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

isDock :: Window -> SUN Bool
isDock = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

isFullscreen :: Window -> SUN Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

isSplash :: Window -> SUN Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

-- | Remove windows from workspaces when they don't actually exist according to X11.
killGhostWins :: SUN ()
killGhostWins = do
  dis <- asks display
  rt <- asks root
  (_,_,qt) <- liftIO $ queryTree dis rt
  aws <- stateFunc getAllWins
  mapM_ (\win -> when (win `notElem` qt) $ removeWindow win) aws

-- | Kill a window. Properly. Thanks XMonad!
killWindow :: SUN ()
killWindow = asks display >>= \dis -> do
  wmdelt <- atomWMDELETEWINDOW
  wmprot <- atomWMPROTOCOLS
  fw <- focusedWin <$> gets focusWS
  ffw <- gets (focusFloat . focusWS)
  liftIO $ when (isJust fw || isJust ffw) $ do
  let w = fromMaybe (fromJust fw) ffw
  protocols <- getWMProtocols dis w
  if wmdelt `elem` protocols
    then allocaXEvent $ \ev -> do
         setEventType ev clientMessage
         setClientMessageEvent ev w wmprot 32 wmdelt 0
         sendEvent dis w False noEventMask ev
    else void (killClient dis w)

-- | Used for when you want to apply a function to the whole state at once.
-- i.e. function with type signature (SUNState -> SUNState)
modifyState :: MonadState s m => (s -> s) -> m ()
modifyState ssf = Control.Monad.State.get >>= \ss -> put (ssf ss)

mouseMove :: Window -> SUN ()
mouseMove w = asks display >>= \dis -> do
  float w
  wa <- liftIO $ getWindowAttributes dis w
  let x = wa_x wa ; y = wa_y wa
  (_,_,_,pox,poy,_,_,_) <- liftIO $ queryPointer dis w
  setMouseDrag $ Just (Move w (fip x) (fip y) (fip pox) (fip poy))

mouseResize :: Window -> SUN ()
mouseResize w = asks display >>= \dis -> do
  float w
  wa <- liftIO $ getWindowAttributes dis w
  liftIO $ warpPointer dis none w 0 0 0 0 (fip (wa_width wa)) (fip (wa_height wa))
  setMouseDrag $ Just (Resize w (fid $ wa_x wa) (fid $ wa_y wa))

moveLeftWS :: SUN ()
moveLeftWS = do
  fwsn <- gets focusWSNum
  nws <- length <$> gets workspaces
  if fwsn == 1
    then changeWS nws
    else changeWS (fwsn - 1)

moveRightWS :: SUN ()
moveRightWS = do
  fwsn <- gets focusWSNum
  nws <- length <$> gets workspaces
  if fwsn == nws
    then changeWS 1
    else changeWS (fwsn + 1)

-- | Moves the currently focused window to another workspace.
moveWinToWS :: Int -> SUN ()
moveWinToWS wsn = modifyState (moveToWS wsn) >> arrange >> refresh >> updateBar >> updateFocus

-- | Remove every frame but the one currently focused
only :: SUN ()
only = do
  fs <- gets (floats . focusWS)
  aws <- (\ws -> getVisWins ws ++ getHidWins ws) <$> gets focusWS
  ffw <- gets (focusFloat . focusWS)
  fw <- focusedWin <$> gets focusWS
  inF <- gets (inFullScreen . focusWS)
  unless inF $ do
    storeUndo
    when (isJust fw && isNothing ffw) $ do
      focus . tree . focusWS =: Frame fw
      hidden . focusWS =: delete (fromJust fw) (aws ++ fs)
    when (isJust ffw) $ do
      focusFloat . focusWS =: Nothing
      floats . focusWS =: []
      focus . tree . focusWS =: Frame ffw
      hidden . focusWS =: delete (fromJust ffw) (aws ++ fs)
    when (isNothing ffw && isNothing fw) $ do
      focus . tree . focusWS =: Frame Nothing
      hidden . focusWS =: (aws ++ fs)
    trail . tree . focusWS =: []
    arrange >> refresh >> updateFocus >> updateBar

-- | Kill X11
quit :: SUN ()
quit = liftIO exitSuccess

-- | Cycles through hidden windows in given direction.
raiseHidden :: Direction -> SUN ()
raiseHidden dir = do
  ff <- isJust <$> gets (focusFloat . focusWS)
  unless ff $ do
    focusWS =. cycleHidden dir
    arrange >> refresh >> updateFocus >> updateBar

readWorkSpace :: String -> SUN ()
readWorkSpace path = do
  tr <- liftIO $ readFile path
  liftIO $ print tr

-- | Map all windows that should be visible, unmap all windows that shouldn't.
refresh :: SUN ()
refresh = asks display >>= \dis -> do
    modifyState updateWorkspace
    vws <- getVisWins <$> gets focusWS ; aws <- stateFunc getAllWins
    fs <- gets (floats . focusWS) ; afs <- map (L.get floats) <$> gets workspaces
    liftIO $ mapM_ (mapWindow dis) $ vws ++ fs
    liftIO $ mapM_ (unmapWindow dis) $ (aws \\ vws) ++ (concat afs \\ fs)
    killGhostWins

-- | Remove currently focused frame from the tree and, if one was present, add the
-- previously focused window to the hidden stack.
removeFrame :: SUN ()
removeFrame = do
  storeUndo
  ffw <- gets (focusFloat . focusWS)
  when (isNothing ffw) $ do
    safeModify focusWS killFrame
    arrange >> refresh >> updateFocus >> updateBar

-- | Kills the current window (floating or not) and
-- removes all traces of it from SUNState
removeWindow :: Window -> SUN ()
removeWindow w = do
  fs <- gets (floats . focusWS)
  ff <- gets (focusFloat . focusWS)
  fw <- focusedWin <$> gets focusWS
  inF <- gets (inFullScreen . focusWS)
  if isJust ff
    then let ffw = fromJust ff in
      when (ffw == w) $ (focusFloat . focusWS) =: Nothing
    else do 
    modifyState (annihilateWin w)
    case fw of
      Just w' -> when (w' == w) $ do
        raiseHidden R
        fw' <- focusedWin <$> gets focusWS
        when (isNothing fw' && not  (null fs))
            $ (focusFloat . focusWS) =: Just (head fs)
      Nothing -> return ()
  (floats . focusWS) =. delete w
  when inF $ (inFullScreen . focusWS) =: False
  arrange >> refresh >> updateBar >> updateFocus

-- | Resizes frame in a specific direction
resizeFrame :: Direction -> SplitRatio -> SUN ()
resizeFrame dir dr = gets (trail . tree . focusWS) >>= \t ->
  when (t /= []) $ do
    storeUndo
    safeModify (tree . focusWS) (resize dir dr) 
    arrange >> refresh >> updateFocus >> updateBar

runSUN :: SUN a -> SUNState -> SUNConf -> IO (Either String a)
runSUN (SUN a) !st !c = evalStateT (runReaderT (runErrorT a) c) st

-- | Make sure a function that modifies state meets certain criteria before
-- actually applying it.  (ex: doesn't create exceedingly small windows)
-- Basically, this prevents the user from doing stupid things.
-- TODO: Think of more criteria to add here (need more beta testers!)
safeModify :: (SUNState :-> a) -> (a -> a) -> SUN ()
safeModify lns f = do
    ss' <- stateFunc (L.modify lns f)
    sw <- gets screenWidth ; sh <- gets screenHeight ; let nt = L.get (tree . focusWS) ss'
    inFS <- gets (inFullScreen . focusWS)
    when (not inFS && all notTooSmall (map fst $ flatten nt sw sh)) $ lns =. f
  where notTooSmall (_,_,w,h) = w > 50 && h > 50

setMouseDrag :: Maybe DragType -> SUN ()
setMouseDrag dragType = do
    rt <- asks root ; dis <- asks display
    _  <- liftIO $ grabPointer dis rt False (buttonReleaseMask .|. pointerMotionMask)
            grabModeAsync grabModeAsync none none currentTime
    dragging =: dragType

-- | Does basic Xlib setup and generates a SUNConf object based on 
-- user-specified settings in a UserConf.
setup :: UserConf -> IO SUNConf
setup !uc = openDisplay [] >>= \dis -> do
  installSignalHandlers
  let scr = defaultScreen dis
      rt  = defaultRootWindow dis
  fc <- getColor dis scr $ L.get focusedBorder uc
  nc <- getColor dis scr $ L.get normalBorder uc
  bg <- getColor dis scr $ L.get background uc
  fg <- getColor dis scr $ L.get foreground uc
  ms <- getModifierMapping dis 
  nmlck <- foldr (.|.) 0 <$> sequence [ do
          ks <- keycodeToKeysym dis kc 0
          if ks == xK_Num_Lock
            then return (setBit 0 (fromIntegral m)) 
            else return (0 :: KeyMask)
          | (m, kcs) <- ms, kc <- kcs, kc /= 0]
  selectInput dis rt $  substructureRedirectMask
                    .|. substructureNotifyMask
                    .|. structureNotifyMask
                    .|. buttonPressMask
  liftIO $ ungrabButton dis anyButton anyModifier rt >> ungrabKey dis anyKey anyModifier rt
  xSetErrorHandler
  hSetBuffering stdout NoBuffering
  sync dis False
  return $ SUNConf dis rt uc nc fc bg fg nmlck

-- | Shift the currently focused window in the specified direction, placing
-- it on top of whatever was already there.
shift :: Direction -> SUN ()
shift dir = do
  ff <- isJust <$> gets (focusFloat . focusWS)
  inF <- gets (inFullScreen . focusWS)
  unless (ff || inF) $ do
    t <- gets (tree . focusWS)
    (sw,sh) <- getScrDims
    let fw = fromFrame t
        nw = fromFrame $ changeFocus dir sw sh t
    when (nw /= fw && isJust fw) $ do
      (tree . focusWS) =. replace Nothing
      raiseHidden R
      (tree . focusWS) =. (replace fw . changeFocus dir sw sh)
      when (isJust nw) $ (hidden . focusWS) =. (fromJust nw :)
      arrange >> refresh >> updateFocus >> updateBar

spawnTerminal :: SUN ()
spawnTerminal = asks (terminal . userConf) >>= spawn

splitH :: SplitRatio -> SUN ()
splitH r = do
  storeUndo
  safeModify focusWS (doSplit $ hSplit r)
  arrange >> refresh >> updateFocus >> updateBar

splitV :: SplitRatio -> SUN ()
splitV r = do
  storeUndo
  safeModify focusWS (doSplit $ vSplit r)
  arrange >> refresh >> updateFocus >> updateBar

-- | Applies a function to the state and returns the result, but
-- does NOT update it in the global state
stateFunc ::  MonadState a m => (a -> b) -> m b
stateFunc ssf = Control.Monad.State.get >>= \ss -> return (ssf ss)

-- | Stores current workspace in the undo history, making sure
-- the undo history doesn't exceed the user-specified size
storeUndo :: SUN ()
storeUndo = do
  nus <- asks (maxUndo . userConf)
  cws <- gets focusWS
  (undoHistory . focusWS) =. (cws :) . take (nus - 1)

-- | Entry point.
sunwm :: UserConf -> IO (Either String ())
sunwm !uc = setup uc >>= runSUN 
    (grabPrefixTops >> updateBar >> configureBarScr 0 >> eventLoop) st
  where st = initState $ length $ L.get wsNames uc

-- | Swap the content (empty or not) of two adjacent frames in specified direction.
swap :: Direction -> SUN ()
swap dir = do
    t <- gets (tree . focusWS)
    (sw,sh) <- getScrDims
    let fw = fromFrame t ; nw = fromFrame $ changeFocus dir sw sh t
    when (nw /= fw) $ do
      storeUndo
      safeModify (tree . focusWS) (replace fw . changeFocus dir sw sh . replace nw)
      arrange >> refresh >> updateFocus >> updateBar

-- | Toggle the current workspace to fullscreen and back
toggleFullScreen :: SUN ()
toggleFullScreen = do
  ffw <- gets (focusFloat . focusWS)
  fw <- focusedWin <$> gets focusWS
  inF <- gets (inFullScreen . focusWS)
  unless (isNothing fw && isNothing ffw) storeUndo
  when (isNothing ffw && isJust fw) $ do
    float $ fromJust fw
    (inFullScreen . focusWS) =: True
  when (isJust ffw && inF) $ do
    unfloat
    (inFullScreen . focusWS) =: False
  when (isJust ffw && not inF) $
    (inFullScreen . focusWS) =: True
  arrange >> refresh >> updateFocus >> updateBar

-- | Switch to last focused workspace
toggleWS :: SUN ()
toggleWS = gets lastWS >>= changeWS

-- | Undo the last action (split, resize, swap, etc)
undo :: SUN ()
undo = do
  un <- gets (undoHistory . focusWS)
  cws <- gets focusWS
  let vws = getVisWins cws
      hws = getHidWins cws
  unless (null un) $ do
    let (u:uss) = un
        vws' = getVisWins u
        hws' = getHidWins u
        fws = (vws ++ hws) \\ (vws' ++ hws')
        fw  = focusedWin u
    focusWS =: u
    (undoHistory . focusWS) =: uss
    (hidden . focusWS) =. (++ fws)
    nhws <- gets (hidden . focusWS)
    when (isNothing fw && not (null nhws)) $ raiseHidden R
    arrange >> refresh >> updateFocus >> updateBar

-- | Unfloat the currently focused window and place it in the tree
unfloat :: SUN ()
unfloat = do
  ffw <- gets (focusFloat . focusWS)
  when (isJust ffw) $ do
    fw <- focusedWin <$> gets focusWS
    when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
    tree . focusWS =. replace ffw
    floats . focusWS =. Data.List.delete (fromJust ffw)
    focusFloat . focusWS =: Nothing
    arrange >> refresh >> updateFocus >> updateBar

-- | Sends formated workspace boxes and window names to stdin of xmobar.
-- TODO: Break this up into multiple functions... it is too dense!
updateBar :: SUN ()
updateBar = asks (barConf . userConf) >>= \c -> do
    modifyState updateWorkspace ; dis <- asks display
    wsns <- asks (wsNames . userConf) ; fw <- focusedWin <$> gets focusWS
    ffw <- gets (focusFloat . focusWS)
    fwsn <- gets focusWSNum ; wss <- gets workspaces
    wTitle <- liftIO $ maybe (return Nothing) (fetchName dis) fw
    fwTitle <- liftIO $ maybe (return Nothing) (fetchName dis) ffw
    let h = L.get handle c ; cur = L.get currentC c ; hid = L.get hiddenC c 
        hidE = L.get hiddenEmptyC c ; titC = L.get titleC c
        wCount ws = length $ getHidWins ws ++ getVisWins ws ++ L.get floats ws
        xmb (fgc,bgc) str = 
          "<fc=" ++ fgc ++ "," ++ bgc ++ ">" ++ " " ++ take 45 str ++ " " ++ "</fc>"
        wsF (ws,n)
            | n == (fwsn-1) = xmb cur (wsns !! n)
            | wCount ws == 0 = xmb hidE (wsns !! n)
            | otherwise = xmb hid (wsns !! n)
        frmtns = concatMap wsF $ zip wss [0..]
        wTitle' = if isJust ffw && isJust fwTitle then xmb titC $ fromJust fwTitle
                  else maybe "" (xmb titC) wTitle
    vwins <- getVisWins <$> gets focusWS >>= \vws -> mapM (liftIO . fetchName dis)
             $ if isNothing ffw && isJust fw then delete (fromJust fw) vws else vws
    hwins <- getHidWins <$> gets focusWS >>= mapM (liftIO . fetchName dis)
    fwins <- gets (floats . focusWS) >>= \fws -> mapM (liftIO . fetchName dis)
             $ if isJust ffw then delete (fromJust ffw) fws else fws
    let vs = map (xmb hid . fromJust) $ filter (/= Nothing) $ vwins ++ fwins
    let hs = map (xmb hidE . fromJust) $ filter (/= Nothing) hwins
    liftIO $ hPutStrLn h $ filter (`elem` ['\32'..'\126']) $ frmtns 
            ++ " " ++ wTitle' ++ " " ++ intercalate "|" (vs ++ hs)

-- | Update the focused window, redraw borders accordingly on all mapped windows.
-- TODO: Clean this beast up a little
updateFocus :: SUN ()
updateFocus = do
    (dis,rt,sw,sh,nc,fc,uc) <- getConf
    ws <- getVisWins <$> gets focusWS
    fw <- focusedWin <$> gets focusWS
    ff <- gets (focusFloat . focusWS)
    fs <- gets (floats . focusWS)
    tl <- gets (trail . tree . focusWS)
    bh <- gets barHeight
    let unFocus win = setWindowBorder dis win nc >> setWindowBorderWidth dis win bw
        bw = L.get borderWidth uc
    liftIO $ clearWindow dis rt
    case ff of 
      (Just ffw) -> do
        inFS <- gets (inFullScreen . focusWS)
        isFS <- isFullscreen ffw
        xGrabButton False ffw
        if inFS || isFS
          then liftIO $ do
            setWindowBorderWidth dis ffw 0
            moveResizeWindow dis ffw 0 0 sw (sh+bh)
          else liftIO $ do
            setWindowBorderWidth dis ffw bw
            setWindowBorder dis ffw fc
        mapM_ (xGrabButton True) $ ws ++ delete ffw fs
        if tl == [] && ws /= []
          then liftIO $ setWindowBorderWidth dis (head ws) 0
          else liftIO $ mapM_ unFocus $ ws ++ delete ffw fs
        liftIO $ setInputFocus dis ffw revertToParent 0
        liftIO $ raiseWindow dis ffw 
      Nothing -> do
        liftIO $ mapM_ (raiseWindow dis) fs
        case fw of
          Just w -> do
            if tl == []
              then liftIO $ setWindowBorderWidth dis w 0
              else do
                liftIO $ setWindowBorderWidth dis w bw
                liftIO $ setWindowBorder dis w fc
            liftIO $ setInputFocus dis w revertToParent 0
            mapM_ (xGrabButton True) $ delete w ws ++ fs
            xGrabButton False w
            liftIO $ mapM_ unFocus (delete w ws ++ fs)
          Nothing -> do
            liftIO $ setInputFocus dis rt revertToParent 0
            when (tl /= []) drawFrameBorder
            liftIO $ mapM_ unFocus $ ws ++ fs
            mapM_ (xGrabButton True) $ ws ++ fs

writeWorkSpace :: String -> SUN ()
writeWorkSpace path = do
  tr <- gets (tree . focusWS)
  liftIO $ writeFile path (show tr)

-- | Grab or ungrab a mouse button
xGrabButton :: Bool -> Window -> SUN ()
xGrabButton grab w = asks display >>= \dis -> do
    nmlck <- asks numlockMask
    liftIO $ if grab
        then forM_ [button1, button2, button3] $ \b ->
               grabButton dis b anyModifier w False buttonPressMask
               grabModeAsync grabModeSync none none
        else do
          ungrabButton dis anyButton anyModifier w 
          forM_ [mod1Mask, mod1Mask .|. nmlck, mod1Mask .|. lockMask
                , mod1Mask .|. nmlck .|. lockMask] $ \m -> do
            grabButton dis button1 m w False buttonPressMask
              grabModeAsync grabModeSync none none
            grabButton dis button3 m w False buttonPressMask
              grabModeAsync grabModeSync none none
