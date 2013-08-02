{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Sunwm.Core
---- Copyright   :  (c) Zac Meadows 2011
---- License     :  WTFPL 2.0 (see LICENSE)
----
---- Maintainer  :  zmeadows@gmail.com
---- Stability   :  unstable
---- Portability :  not portable
----
---- Interaction with XLib and STree.hs
----
-----------------------------------------------------------------------------

module Sunwm.Core where

import Sunwm.STree
import Sunwm.Util
import Sunwm.FocusMap

import Prelude hiding ((.), id)
import Control.Category ((.))

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Reader hiding (asks)
import Control.Monad.Error

import Data.Maybe
import Data.List ((\\), find, nub)
import Data.Label ((:->))
import Data.Label.PureM ((=:),(=.),gets,asks)
import qualified Data.Label as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bits hiding (shift)

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Foreign.C.Types (CLong)

import System.IO
import System.Exit

data UserConf = UserConf
    { _normalBorder  :: !String
    , _focusedBorder :: !String
    , _borderWidth   :: !Dimension
    , _keyBinds      :: !(Map (KeyMask, KeySym) (SUN ()))
    , _topKeyBinds   :: !(Map (KeyMask, KeySym) (SUN ()))
    , _wsNames       :: ![String]
    , _prefixKey     :: !(KeyMask, KeySym)
    , _terminal      :: !String
    , _initHook      :: SUN ()
    , _stackHook     :: SUN ()
    }

-- data MouseFocusStyle = None | Click | Follow

data SUNConf = SUNConf
    { _display            :: !Display
    , _root               :: !Window
    , _userConf           :: !UserConf
    , _normalBorderColor  :: !Pixel
    , _focusedBorderColor :: !Pixel
    , _numlockMask        :: !KeyMask
    }

newtype SUN a = SUN (ErrorT String (ReaderT SUNConf (StateT SUNState IO)) a)
    deriving (Monad, MonadPlus, MonadIO, MonadState SUNState, MonadReader SUNConf, Functor)

$(L.mkLabels [''SUNConf, ''UserConf])

-- | ---------------------- | --
-- | SETUP/EXTRANEOUS STUFF | --
-- | ---------------------- | --

-- | Move mouse to far bottom right corner of screen.
-- TODO: fix for multi monitor setups
banish :: SUN ()
banish = do
    dis     <- asks display
    rt      <- asks root
    (sw,sh) <- getFocusScrDims
    (x,y)   <- getFocusScrPos
    liftIO $ warpPointer dis none rt 0 0 0 0 (x + fip sw) (y + fip sh)

-- | Gives the dimensions of the currently focused screen
getFocusScrDims :: SUN (Dimension,Dimension)
getFocusScrDims = (L.get width &&& L.get height) <$> gets focusScr

-- | Gives the position of the currently focused screen
getFocusScrPos :: SUN (Position,Position)
getFocusScrPos = (L.get xPos &&& L.get yPos) <$> gets focusScr

-- | Does basic Xlib setup and generates a SUNConf object based on
-- user-specified settings in a UserConf.
setup :: UserConf -> IO SUNConf
setup !uc = do
  dis <- openDisplay ""
  installSignalHandlers
  let defRt = defaultRootWindow dis
  fc <- getColor dis $ L.get focusedBorder uc
  nc <- getColor dis $ L.get normalBorder uc
  ms <- getModifierMapping dis
  nmlck <- foldr (.|.) 0 <$> sequence [ do
          ks <- keycodeToKeysym dis kc 0
          return $ if ks == xK_Num_Lock
            then setBit 0 (fromIntegral m)
            else 0 :: KeyMask
          | (m, kcs) <- ms, kc <- kcs, kc /= 0]
  selectInput dis defRt $ substructureRedirectMask .|. substructureNotifyMask .|.
                          structureNotifyMask -- .|. buttonPressMask .|. buttonReleaseMask
  liftIO $ ungrabButton dis anyButton anyModifier defRt
  liftIO $ ungrabKey dis anyKey anyModifier defRt
  xSetErrorHandler
  hSetBuffering stdout NoBuffering
  sync dis False
  return $ SUNConf dis defRt uc nc fc nmlck

getColor :: Display -> String -> IO Pixel
getColor dis !str = let cMap = defaultColormap dis (defaultScreen dis) in
            (color_pixel . fst) <$> allocNamedColor dis cMap str

arrange :: SUN ()
arrange = keys . L.get screens <$> get >>= mapM_ arrangeN

-- | Configure the sizes of all mapped windows according to current tree shape.
arrangeN :: Int -> SUN ()
arrangeN n = asks display >>= \dis -> do
    t <- L.get tree <$> focused <$> gets (workspaces . screenN n)
    (sx,sy) <- (L.get xPos &&& L.get yPos) <$> gets (screenN n)
    (sw,sh) <- (L.get width &&& L.get height) <$> gets (screenN n)
    ds <- L.get docks <$> gets (screenN n)
    bw <- (2 *) <$> asks (borderWidth . userConf)

    let ws = flattenToDimWins sw sh ds t
        putWindow ((x,y,w,h),win) = moveResizeWindow dis win (x + sx) (y + sy) (w - bw) (h - bw)
    when (length ws > 1 || L.get trail t /= []) $ liftIO $ mapM_ putWindow ws
    when (length ws == 1 && null (L.get trail t)) $ do
        let ((x,y,w,h),win) = head ws
        liftIO $ moveResizeWindow dis win (sx + x) (sy + y) w h

-- | Map all windows that should be visible, unmap all windows that shouldn't.
-- TODO: make this more efficient (only map things that aren't already mapped, same for unmapped)
-- or does x11 already account for this?
refresh :: SUN ()
refresh = do
    dis <- asks display
    scrs <- elems <$> gets screens
    let vs = concatMap getScrVisWins scrs
        hs = concatMap getScrAllWins scrs \\ vs
    ioMap_ (unmapWindow dis) hs >> ioMap_ (mapWindow dis) vs

runStackHook :: SUN ()
runStackHook = asks userConf >>= L.get stackHook

ioMap_ :: MonadIO m => (a -> IO b) -> [a] -> m ()
ioMap_ f !l = liftIO $ mapM_ f l

ioMap :: MonadIO m => (a -> IO b) -> [a] -> m [b]
ioMap f !l = liftIO $ mapM f l

-- | Strip numlock && capslock from a mask
cleanMask :: KeyMask -> SUN KeyMask
cleanMask km = asks numlockMask >>= \nml ->
  return (complement (nml .|. lockMask) .&. km)

focusedWin :: SUN (Maybe Window)
focusedWin = fromFrame <$> gets (tree . focusWS)

updateFocus :: SUN ()
updateFocus = do
    dis <- asks display
    rt <- asks root
    liftIO $ clearWindow dis rt
    scrns <- keys <$> gets screens
    mapM_ updateFocusN scrns

-- | TODO: donm't draw bordered on sole window if on other screen
updateFocusN :: Int -> SUN ()
updateFocusN scrn = do
    dis <- asks display
    rt <- asks root
    nc <- asks normalBorderColor
    fc <- asks focusedBorderColor
    bw <- asks (borderWidth . userConf)
    scr <- flip (<!>) scrn <$> gets screens
    fscn <- fst <$> gets screens

    let unFocus win = liftIO $ setWindowBorder dis win nc >> setWindowBorderWidth dis win bw

    let ws = getScrAllWins scr
        fwks = focused $ L.get workspaces scr
        fw = fromFrame $ L.get tree fwks
        tl = L.get (trail . tree) fwks

    ioMap_ unFocus ws

    case fw of
      Just w -> do
        liftIO $ if null tl
          then setWindowBorderWidth dis w 0
          else when (scrn == fscn) $ setWindowBorderWidth dis w bw >> setWindowBorder dis w fc
        when (scrn == fscn) $ liftIO $ setInputFocus dis w revertToParent 0
      Nothing -> do
        when (scrn == fscn) $ liftIO $ setInputFocus dis rt revertToParent 0
        when (tl /= []) drawFrameBorder
        ioMap_ unFocus ws

drawFrameBorder :: SUN ()
drawFrameBorder = do
    dis <- asks display
    rt <- asks root
    (sw,sh) <- getFocusScrDims
    (sx,sy) <- (L.get xPos &&& L.get yPos) <$> gets focusScr
    fc <- asks focusedBorderColor
    bw <- asks (borderWidth . userConf)
    ds <- gets (docks . focusScr)

    t <- gets (tree . focusWS)
    let mt = find ((==) t . snd) $ flatten t ds sw sh

    when (isJust mt && L.get trail t /= []) $ liftIO $ do
        let ((x,y,w,h),_) = fromJust mt
        gc <- createGC dis rt
        setLineAttributes dis gc (fi bw) lineDoubleDash capButt joinMiter
        setForeground dis gc fc
        drawRectangle dis rt gc (fi x + div (fip bw) 2 + sx) (fi y + div (fip bw) 2 + sy) (w - fid bw) (h - fid bw)
        sync dis False
        freeGC dis gc

react :: SUN () -> SUN ()
react sun = sun >> arrange >> refresh >> updateFocus >> runStackHook

-- | Swap the content (empty or not) of two adjacent frames in specified direction.
swapToDir :: Direction -> SUN ()
swapToDir dir = react $ screens =. swap dir

swapWStoScr :: Int -> SUN ()
swapWStoScr n = sloppyGuard $ react $ screens =. swapWSscr n

-- | Make sure a function that modifies state meets certain criteria before
-- actually applying it.  (ex: doesn't create exceedingly small windows)
-- Basically, this prevents the user from doing stupid things.
-- TODO: Think of more criteria to add here (need more beta testers!)
safeModify :: (SUNState :-> a) -> (a -> a) -> SUN ()
safeModify lns f = do
    ss' <- L.modify lns f <$> get
    (sw,sh) <- getFocusScrDims
    let nt = L.get (tree . focusWS) ss'
    inFS <- gets (inFullScreen . focusWS)
    ds <- gets (docks . focusScr)
    when (not inFS && all notTooSmall (map fst $ flatten nt ds sw sh)) $ lns =. f
  where notTooSmall (_,_,w,h) = w > 50 && h > 50

runSUN :: SUN a -> SUNState -> SUNConf -> IO (Either String a)
runSUN (SUN a) !st !c = evalStateT (runReaderT (runErrorT a) c) st

-- | Resizes frame in a specific direction
resizeFrame :: Direction -> SplitRatio -> SUN ()
resizeFrame dir dr = react $ do
  t <- gets (trail . tree . focusWS)
  when (t /= []) $ safeModify (tree . focusWS) (resize dir dr)

-- | Remove currently focused frame from the tree and, if one was present, add the
-- previously focused window to the hidden stack.
removeFrame :: SUN ()
removeFrame = react $ safeModify focusWS killFrame

-- | Kill X11
quit :: SUN ()
quit = liftIO exitSuccess

-- | Remove every frame but the one currently focused
--  TODO: this causing fast map/remap high cpu bug?
makeOnly :: SUN ()
makeOnly = react $ focusWS =. only

-- | Kills the current window (floating or not) and
-- removes all traces of it from SUNState
removeWindow :: Window -> SUN ()
removeWindow w = react $ do
  fw <- focusedWin
  when (isJust fw) $ when (fromJust fw == w) $ raiseHidden R
  screens =. annihilateWin w
  inF <- gets (inFullScreen . focusWS)
  when inF $ (inFullScreen . focusWS) =: False

-- | Remove windows from workspaces when they don't actually exist according to X11.
-- | TODO: make sure queryTree gets windows on all screens
-- this is broken at the moment! no idea why.
killGhostWins :: SUN ()
killGhostWins = do
  dis <- asks display
  rt <- asks root
  (_,_,qt) <- liftIO $ queryTree dis rt
  aws <- getAllWins
  mapM_ (\win -> when (win `notElem` qt) $ removeWindow win) aws

getAllWins :: SUN [Window]
getAllWins = concat <$> mapElems getScrAllWins <$> gets screens

-- | Kill a window. Properly. Thanks XMonad!
killWindow :: SUN ()
killWindow = asks display >>= \dis -> do
  wmdelt <- atomWMDELETEWINDOW
  wmprot <- atomWMPROTOCOLS
  fw <- focusedWin
  liftIO $ when (isJust fw) $ do
    let w = fromJust fw
    protocols <- getWMProtocols dis w
    if wmdelt `elem` protocols
      then allocaXEvent $ \ev -> do
           setEventType ev clientMessage
           setClientMessageEvent ev w wmprot 32 wmdelt 0
           sendEvent dis w False noEventMask ev
      else void (killClient dis w)

-- | Common non-predefined atoms
atomWMDELETEWINDOW, atomWMPROTOCOLS, atomWMSTATE :: SUN Atom
atomWMDELETEWINDOW = getAtom "WM_DELETE_WINDOW"
atomWMPROTOCOLS    = getAtom "WM_PROTOCOLS"
atomWMSTATE        = getAtom "WM_STATE"

-- | Cycles through hidden windows in given direction.
raiseHidden :: Direction -> SUN ()
raiseHidden dir = react $ focusWS =. cycleHidden dir

-- | Switch to another workspace
changeWS :: Int -> SUN ()
changeWS wsn = sloppyGuard $ react $ do
  fwsn <- fst <$> gets (workspaces . focusScr)
  when (fwsn /= wsn) $ do
    lastWS . focusScr =: fwsn
    focusScr =. changeWorkspace wsn

changeScr :: Int -> SUN ()
changeScr scrn = react $ do
  fscrn <- fst <$> gets screens
  when (scrn /= fscrn) $ do
    lastScr =: fscrn
    modify $ changeScreen scrn

toggleWS :: SUN ()
toggleWS = gets (lastWS . focusScr) >>= changeWS

toggleScr :: SUN ()
toggleScr = gets lastScr >>= changeScr

isDialog :: Window -> SUN Bool
isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

isDock :: Window -> SUN Bool
isDock = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

isFullscreen :: Window -> SUN Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

isSplash :: Window -> SUN Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isUtility :: Window -> SUN Bool
isUtility = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY"

isMenu :: Window -> SUN Bool
isMenu = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

isInProperty :: String -> String -> Window -> SUN Bool
isInProperty p v w = do
  va <- getAtom v
  r <- getProp32s p w
  return $ case r of
    Just xs -> fromIntegral va `elem` xs
    _ -> False

-- | Wrapper for the common case of atom internment
getAtom :: String -> SUN Atom
getAtom str = asks display >>= \dis -> liftIO $ internAtom dis str False

-- | Get a window property from atom
getProp32 :: Atom -> Window -> SUN (Maybe [CLong])
getProp32 a w = asks display >>= \dis -> liftIO $ getWindowProperty32 dis a w

-- | Get a window property from string
getProp32s :: String -> Window -> SUN (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }

focusTo :: Direction -> SUN ()
focusTo dir = react $ do
    pscr <- fst <$> gets screens
    safeModify screens (changeFocus dir)
    nscr <- fst <$> gets screens
    when (pscr /= nscr) $ lastScr =: pscr

shiftTo :: Direction -> SUN ()
shiftTo dir = react $ do
    pscr <- fst <$> gets screens
    safeModify screens (shift dir)
    nscr <- fst <$> gets screens
    when (pscr /= nscr) $ lastScr =: pscr

spawnTerminal :: SUN ()
spawnTerminal = asks (terminal . userConf) >>= spawn

moveWinToWS :: Int -> SUN ()
moveWinToWS wsn = react $ (workspaces . focusScr) =. moveToWS wsn

moveWinToScr :: Int -> SUN ()
moveWinToScr scn = sloppyGuard $ react $ screens =. moveToScr scn

sloppyGuard :: SUN () -> SUN ()
sloppyGuard sun = do
    dis <- asks display
    aws <- getAllWins
    ioMap_ (\w -> selectInput dis w 0) aws
    sun
    ioMap_ (\w -> selectInput dis w clientMask) aws

-- | Grab prefix key and top-level bindings.
grabPrefixTops :: SUN ()
grabPrefixTops = do
    dis <- asks display
    rt <- asks root
    ks <- M.keys <$> asks (topKeyBinds . userConf)
    liftIO $ ungrabKey dis anyKey anyModifier rt
    let xGrabKey (km,kc) = grabKey dis kc km rt True grabModeSync grabModeAsync
        toKeyCode (mask,sym) = do
            kc <- keysymToKeycode dis sym
            return (mask,kc)
    ks' <- liftIO $ mapM toKeyCode ks
    p <- asks (prefixKey . userConf) >>= liftIO . toKeyCode
    liftIO $ xGrabKey p >> mapM_ xGrabKey ks'

-- | Rotate current layout by 90 degrees
flipT :: SUN ()
flipT = react $ safeModify (tree . focusWS) flipTree

-- | Make all split ratios in the tree 0.5
equalize :: SUN ()
equalize = react $ safeModify (tree . focusWS) makeEqual

splitH :: SplitRatio -> SUN ()
splitH r = react $ safeModify focusWS (doSplit $ hSplit r)

splitV :: SplitRatio -> SUN ()
splitV r = react $ safeModify focusWS (doSplit $ vSplit r)

detectDocks :: SUN ()
detectDocks = do
    dis <- asks display ; rt <- asks root
    (_,_,qt) <- liftIO $ queryTree dis rt
    qt' <- nub <$> filterM isDock qt
    screens =. mapF (L.set docks [])
    mapM_ addDock qt'

addDock :: Window -> SUN ()
addDock win = do
    (x,y,w,h) <- getWinPosDim win
    scrNum <- fromJust <$> screenOfPoint (x + fip (div w 2), y + fip (div h 2)) <$> gets screens
    (sx,sy) <- (L.get xPos &&& L.get yPos) <$> gets (screenN scrNum)
    struts <- fromJust <$> getProp32s "_NET_WM_STRUT_PARTIAL" win
    (docks . screenN scrNum) =. (constructDock win (x - sx) (y - sy) w h struts :)

-- | TODO: return Maybe, in case window doesn't exist
getWinPosDim :: Window -> SUN (Position,Position,Dimension,Dimension)
getWinPosDim win = do
    dis <- asks display
    att <- liftIO $ getWindowAttributes dis win
    return (fip $ wa_x att, fip $ wa_y att, fid $ wa_width att, fid $ wa_height att)

-- | Receives the events and sends them to eventDispatch.
eventLoop :: SUN ()
eventLoop = forever $ asks display >>= \dis -> do
  evt <- liftIO $ do
    sync dis False
    allocaXEvent $ \e -> do
      nextEvent dis e
      getEvent e
  eventDispatch evt

eventDispatch :: Event -> SUN ()

eventDispatch !(UnmapEvent {ev_window = w, ev_send_event = synthetic}) =
    if synthetic
        then removeWindow w
        else detectDocks

-- | TODO: deal with splash/dialog etc (wait until float support added)
eventDispatch !(MapRequestEvent {ev_window = win}) = do
    dis <- asks display
    fw <- focusedWin
    liftIO $ selectInput dis win clientMask
    allWins <- getAllWins
    isF <- isFullscreen win ; isDia <- isDialog win ; isS <- isSplash win
    isDk <- isDock win
    unless (win `elem` allWins || isF || isDia || isS || isDk) $ react $ do
      when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
      (tree . focusWS) =. replace (Just win)
    when (isDk || isDia || isS || isF) $ liftIO $ mapWindow dis win
    when isDia $ do
        liftIO $ print "dialog detected"
        liftIO $ setInputFocus dis win revertToParent 0
    when isDk $ detectDocks >> arrange >> refresh

-- | TODO: check if detectDocks is actually needed here
eventDispatch !(DestroyWindowEvent {ev_window = w}) = removeWindow w >> detectDocks

eventDispatch evt@(ConfigureRequestEvent _ _ _ dis _ win x y w h bw a d vm) = react $ do
    ws  <- flattenToWins <$> gets (tree . focusWS)
    wa <- liftIO $ getWindowAttributes dis win
    liftIO $ if win `notElem` ws
        then configureWindow dis win vm $ WindowChanges x y w h bw a d
        else allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev win win
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width evt) none
                     (wa_override_redirect wa)
                 sendEvent dis win False 0 ev
    liftIO $ sync dis False
    detectDocks

eventDispatch !evt@(KeyEvent {ev_event_type = et}) = when (et == keyPress) $ do
    dis <- asks display
    rt <- asks root
    inP <- gets inPrefix
    p <- asks (prefixKey . userConf)
    let km = ev_state evt
        kc = ev_keycode evt
        makeCursor c = liftIO $ createFontCursor dis c
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

eventDispatch !(CrossingEvent {ev_window = w, ev_mode = em, ev_event_type = et}) =
    when (et == enterNotify && em == notifyNormal) $ do
      aws <- getAllWins
      when (w `elem` aws) $ react $ screens =. focusToWin w

eventDispatch !(PropertyEvent {}) = runStackHook

-- | Ignore all other event types for which specific behavior isn't defined
eventDispatch !evt
    | ev_event_type evt == configureNotify = detectDocks >> arrange >> refresh
    | otherwise = return ()

clientMask :: EventMask
clientMask = propertyChangeMask .|. enterWindowMask

generateScrBinds :: Int -> SUNConf -> IO SUNConf
generateScrBinds 1 uc = return uc
generateScrBinds ns uc = do
    let screenMoveBinds = map (\(k,n) -> ((mod4Mask, k), changeScr n))
                     $ zip [xK_1..] [1..ns]
        screenSwapBinds = map (\(k,n) -> ((mod4Mask .|. controlMask, k), swapWStoScr n))
                     $ zip [xK_1..] [1..ns]
        screenSendBinds = map (\(k,n) -> ((mod4Mask .|. shiftMask, k), moveWinToScr n))
                     $ zip [xK_1..] [1..ns]
    return $ L.modify (topKeyBinds . userConf)
        (\tkb -> M.union tkb $ M.fromList $ screenMoveBinds ++ screenSendBinds ++ screenSwapBinds) uc

sunwm :: UserConf -> IO (Either String ())
sunwm !uc = do
    sc <- setup uc
    scrRecs <- getScreenInfo $ L.get display sc
    let st = initState (length $ L.get wsNames uc) scrRecs
    sc' <- generateScrBinds (length scrRecs) sc
    runSUN (grabPrefixTops >> L.get (initHook . userConf) sc' >> detectDocks >> eventLoop) st sc'
