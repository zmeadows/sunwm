{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}
module Core where

import STree
import Util
import FocusMap

import Prelude hiding ((.), id)
import Control.Category ((.))
import Data.Maybe
import Data.List ((\\), find, delete, intercalate)
import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Reader hiding (asks)
import Control.Monad.Error
import Control.Concurrent (threadDelay)

import Data.Label ((:->))
import qualified Data.Label as L
import Data.Label.PureM ((=:),(=.),gets,asks)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bits

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Foreign.C.Types (CLong)

import System.IO
import System.Exit

-- data MouseFocusStyle = None | Click | Follow

data UserConf = UserConf
    { _normalBorder  :: !String
    , _focusedBorder :: !String
    , _borderWidth   :: !Dimension
    , _keyBinds      :: !(Map (KeyMask, KeySym) (SUN ()))
    , _topKeyBinds   :: !(Map (KeyMask, KeySym) (SUN ()))
    , _wsNames       :: ![String]
    , _prefixKey     :: !(KeyMask, KeySym)
    , _barConf       :: !(Maybe BarConf)
    , _terminal      :: !String
    }

data BarConf = BarConf
    { _focusColor       :: !(String, String)
    , _hiddenColor      :: !(String, String)
    , _hiddenEmptyColor :: !(String, String)
    , _titleColor       :: !(String, String)
    } deriving (Show, Eq)

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

$(L.mkLabels [''SUNConf, ''BarConf, ''UserConf])

-- | ---------------------- | --
-- | SETUP/EXTRANEOUS STUFF | --
-- | ---------------------- | --

-- | Move mouse to far bottom right corner of screen.
banish :: SUN ()
banish = do
    dis     <- asks display
    rt      <- asks root
    bh      <- fip <$> gets barHeight
    (sw,sh) <- getFocusScrDims
    liftIO $ warpPointer dis none rt 0 0 0 0 (fi sw) $ fi sh + bh

-- | Gives the dimensions of the currently focused screen
getFocusScrDims :: SUN (Dimension,Dimension)
getFocusScrDims = (L.get width &&& L.get height) <$> gets focusScr

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
          if ks == xK_Num_Lock
            then return (setBit 0 (fromIntegral m))
            else return (0 :: KeyMask)
          | (m, kcs) <- ms, kc <- kcs, kc /= 0]
  selectInput dis defRt $ substructureRedirectMask .|. substructureNotifyMask .|.
                          structureNotifyMask .|. buttonPressMask .|. buttonReleaseMask
                          .|. enterWindowMask .|. leaveWindowMask
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
    bw <- (2 *) <$> asks (borderWidth . userConf)
    bh <- fip <$> gets barHeight
    let ws = flattenToDimWins sw (sh - (fi bh)) t
        putWindow ((x,y,w,h),win) = moveResizeWindow dis win (x + sx) (y + bh + sy) (w - bw) (h - bw)
    when (length ws > 1 || L.get trail t /= []) $ liftIO $ mapM_ putWindow ws
    when (length ws == 1 && L.get trail t == []) $
        liftIO $ moveResizeWindow dis (snd $ head ws) sx (bh + sy) sw (sh - (fi bh))

-- | Map all windows that should be visible, unmap all windows that shouldn't.
-- TODO: make this more efficient (only map things that aren't already mapped, same for unmapped)
-- or does x11 already account for this?
refresh :: SUN ()
refresh = do
    --killGhostWins
    dis <- asks display
    scrs <- elems <$> gets screens
    let vs = concatMap getScrVisWins scrs
        hs = (concatMap getScrAllWins scrs) \\ vs
    ioMap_ (unmapWindow dis) hs >> ioMap_ (mapWindow dis) vs

ioMap_ :: MonadIO m => (a -> IO b) -> [a] -> m ()
ioMap_ f !l = liftIO $ mapM_ f l

ioMap :: MonadIO m => (a -> IO b) -> [a] -> m [b]
ioMap f !l = liftIO $ mapM f l

-- | Strip numlock && capslock from a mask
cleanMask :: KeyMask -> SUN KeyMask
cleanMask km = asks numlockMask >>= \nml ->
  return (complement (nml .|. lockMask) .&. km)

formatStrXMobar :: (String,String) -> String -> String
formatStrXMobar (fg,bg) !str =
    "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ " " ++ str ++ " " ++ "</fc>"

getWinTitles :: [Window] -> SUN [String]
getWinTitles !wins = asks display >>= \dis -> catMaybes <$> ioMap (fetchName dis) wins

focusedWin :: SUN (Maybe Window)
focusedWin = fromFrame <$> gets (tree . focusWS)

updateBars :: SUN ()
updateBars = (keys <$> gets screens) >>= mapM_ updateBarN

-- | Sends formated workspace boxes and window names to stdin of xmobar.
updateBarN :: Int -> SUN ()
updateBarN n = asks display >>= \dis -> do
    wsns  <- asks (wsNames . userConf)
    BarConf focC hidC ehidC titC <- fromJust <$> asks (barConf . userConf)
--
    fw <- fromFrame <$> L.get tree <$> focused <$> gets (workspaces . screenN n)
    fwsn <- fst <$> gets (workspaces . screenN n)
    wss  <- elems <$> gets (workspaces . screenN n)
    vs   <- flattenToWins <$> L.get tree <$> focused <$> gets (workspaces . screenN n)
    hs   <- L.get hidden <$> focused <$> gets (workspaces . screenN n)
--
    let wsFormat (n,ws)
            | n == (fwsn-1) = formatStrXMobar focC (wsns !! n)
            | length (getWSAllWins ws) > 0  = formatStrXMobar hidC (wsns !! n)
            | otherwise      = formatStrXMobar ehidC (wsns !! n)
        fwsns = concatMap wsFormat $ zip [0..] wss
        isChar c = c `elem` ['\32'..'\126']
--
    visWinTitles <- map (formatStrXMobar hidC) <$> getWinTitles (maybe vs (delete `flip` vs) fw)
    hidWinTitles <- map (formatStrXMobar ehidC) <$> getWinTitles hs
    focusTitle <- fmap (maybe "" (formatStrXMobar titC)) $ liftIO $ maybe (return Nothing) (fetchName dis) fw
    putXMobarStr n $ filter isChar $ fwsns ++ " " ++ focusTitle ++ intercalate "|" (visWinTitles ++ hidWinTitles)
    return ()

putXMobarStr :: Int -> String -> SUN ()
putXMobarStr n str = do
  bh <- gets (barHandle . screenN n)
  liftIO $ hPutStrLn (fromJust bh) str

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
        if null tl
          then liftIO $ setWindowBorderWidth dis w 0
          else liftIO $
            when (scrn == fscn) $ do
              setWindowBorderWidth dis w bw
              setWindowBorder dis w fc
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
    bh <- gets barHeight

    t <- gets (tree . focusWS)
    let mt = find ((==) t . snd) $ flatten t sw (sh - (fi bh))

    when (isJust mt && (L.get trail t) /= []) $ liftIO $ do
        let ((x,y,w,h),_) = fromJust mt
        gc <- createGC dis rt
        setLineAttributes dis gc (fi bw) lineDoubleDash capButt joinMiter
        setForeground dis gc fc
        drawRectangle dis rt gc (fi x + div (fip bw) 2 + sx) (fi y + div (fip bw) 2 + fip bh + sy) (w - fid bw) (h - fid bw)
        sync dis False
        freeGC dis gc

react :: SUN () -> SUN ()
react sun = sun >> arrange >> refresh >> updateFocus >> updateBars

-- | Swap the content (empty or not) of two adjacent frames in specified direction.
--swap :: Direction -> SUN ()
--swap dir = react $ do
--    t <- gets (tree . focusWS)
--    (sw,sh) <- getFocusScrDims
--    let fw = fromFrame t
--        nw = fromFrame $ changeFocus dir sw sh t
--    when (nw /= fw) $ safeModify (tree . focusWS) (replace fw . changeFocus dir sw sh . replace nw)

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
    when (not inFS && all notTooSmall (map fst $ flatten nt sw sh)) $ lns =. f
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
only :: SUN ()
only = react $ do
  aws <- getScrAllWins <$> gets focusScr
  fw <- focusedWin
  inF <- gets (inFullScreen . focusWS)
  unless inF $ do
    when (isJust fw) $ do
      focus . tree . focusWS =: Frame fw
      hidden . focusWS =: delete (fromJust fw) aws
    when (isNothing fw) $ do
      focus . tree . focusWS =: Frame Nothing
      hidden . focusWS =: aws
    trail . tree . focusWS =: []

-- | Kills the current window (floating or not) and
-- removes all traces of it from SUNState
removeWindow :: Window -> SUN ()
removeWindow w = react $ do
  fw <- focusedWin
  when (isJust fw) $ when (fromJust fw == w) $ raiseHidden R
  modify (annihilateWin w)
  inF <- gets (inFullScreen . focusWS)
  when inF $ (inFullScreen . focusWS) =: False

-- | Remove windows from workspaces when they don't actually exist according to X11.
-- | TODO: make sure queryTree gets windows on all screens
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
changeWS wsn = react $ do
  fwsn <- fst <$> gets (workspaces . focusScr)
  when (fwsn /= wsn) $ do
    (lastWS . focusScr) =: fwsn
    (focusScr =. changeWorkspace wsn)

changeScr :: Int -> SUN ()
changeScr scrn = react $ do
  fscrn <- fst <$> gets screens
  when (scrn /= fscrn) $ do
    lastScr =: fscrn
    modify $ changeScreen scrn

toggleWS = gets (lastWS . focusScr) >>= changeWS

toggleScr = gets lastScr >>= changeScr

isDialog :: Window -> SUN Bool
isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

isDock :: Window -> SUN Bool
isDock = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

isFullscreen :: Window -> SUN Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

isSplash :: Window -> SUN Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

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
focusTo dir = react $ safeModify screens (changeFocus2 dir)

spawnTerminal :: SUN ()
spawnTerminal = asks (terminal . userConf) >>= spawn

moveWinToWS :: Int -> SUN ()
moveWinToWS wsn = react $ (workspaces . focusScr) =. moveToWS wsn

-- | Shift the currently focused window in the specified direction, placing
-- it on top of whatever was already there.
--shift :: Direction -> SUN ()
--shift dir = do
--  inF <- gets (inFullScreen . focusWS)
--  unless inF $ react $ do
--    t <- gets (tree . focusWS)
--    (sw,sh) <- getFocusScrDims
--    let fw = fromFrame t
--        nw = fromFrame $ changeFocus dir sw sh t
--    when (nw /= fw && isJust fw) $ do
--      (tree . focusWS) =. replace Nothing
--      raiseHidden R
--      (tree . focusWS) =. (replace fw . changeFocus dir sw sh)
--      when (isJust nw) $ (hidden . focusWS) =. (fromJust nw :)

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

initBars :: SUN ()
initBars = do
   barEnabled <- isJust <$> asks (barConf . userConf)
   -- TODO: check if xmobar is in PATH
   when barEnabled $ do
       screenNumList <- keys <$> gets screens
       barHandles <- ioMap (\sn -> spawnPipe $ "xmobar -o -x " ++ sn) $ map (\n -> show (n - 1))screenNumList
       liftIO $ threadDelay 200000
       --  TODO: check if launch of xmobar fails
       let assocHandle (scrNum,h) = (barHandle . screenN scrNum) =: Just h
       mapM assocHandle $ zip screenNumList barHandles
       liftIO $ threadDelay 100000
       dis <- asks display ; rt <- asks root
       (_,_,qt) <- liftIO $ queryTree dis rt
       qt' <- filterM isDock qt
       bwa <- liftIO $ getWindowAttributes dis (head qt')
       let bh = fid $ wa_height bwa + wa_y bwa
       barHeight =: bh

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

-- | Core function of the whole window manager.  Receives the events
-- and sends them to eventDispatch.
eventLoop :: SUN ()
eventLoop = forever $ asks display >>= \dis -> do
  evt <- liftIO $ do
    sync dis False
    allocaXEvent $ \e -> do
      nextEvent dis e
      getEvent e
  eventDispatch evt

eventDispatch :: Event -> SUN ()

eventDispatch !(UnmapEvent {ev_window = w, ev_send_event = synthetic}) = when synthetic (removeWindow w)

-- | TODO: deal with splash/dialog etc (wait until float support added)
eventDispatch !(MapRequestEvent {ev_window = win}) = react $ do
    dis <- asks display
    fw <- focusedWin
    liftIO $ selectInput dis win clientMask
    allWins <- getAllWins
    isF <- isFullscreen win ; isDia <- isDialog win ; isS <- isSplash win
    isDk <- isDock win
    unless (win `elem` allWins || isF || isDia || isS || isDk) $ do
      when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
      (tree . focusWS) =. replace (Just win)
    when (isDk || isDia || isS) $ liftIO $ mapWindow dis win

eventDispatch !(DestroyWindowEvent {ev_window = w}) = removeWindow w

eventDispatch !evt@(ConfigureRequestEvent _ _ _ dis _ win x y w h bw a d vm) = react $ do
    ws  <- flattenToWins <$> gets (tree . focusWS)
    wa <- liftIO $ getWindowAttributes dis win
    if win `notElem` ws
        then liftIO $ configureWindow dis win vm $ WindowChanges x y w h bw a d
        else liftIO $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev win win
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width evt) none
                     (wa_override_redirect $ wa)
                 sendEvent dis win False 0 ev
    liftIO $ sync dis False

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

eventDispatch !evt@(CrossingEvent {ev_window = w, ev_mode = em, ev_event_type = et}) =
    when (et == enterNotify && em == notifyNormal) $ react $ screens =. focusToWin w

-- | Ignore all other event types for which we haven't defined specific behavior
eventDispatch !_ = return ()

clientMask :: EventMask
clientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

sunwm :: UserConf -> IO (Either String ())
sunwm !uc = do
    conf <- setup uc
    scrRecs <- getScreenInfo $ L.get display conf
    let st = initState (length $ L.get wsNames uc) scrRecs
    runSUN (initBars >> grabPrefixTops >> updateBars >> eventLoop) st conf
