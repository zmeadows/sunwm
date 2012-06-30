{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}
module Core where

import STree
import Util

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CLong, CInt)

import Control.Monad.State hiding (gets)
import Control.Monad.Reader hiding (asks)
import Data.Bits
import Data.List (delete, (\\), find, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M (Map(..), fromList, lookup, keys, insert, union, map,delete)

import Control.Category
import Prelude hiding ((.), id)
import Data.Label as L
import Data.Label.PureM as P

import System.IO
import System.Exit

newtype SUN a = SUN (ReaderT SUNConf (StateT SUNState IO) a)
    deriving (Monad, MonadPlus, MonadIO, MonadState SUNState, MonadReader SUNConf, Functor)

data SUNConf = SUNConf
    { _display            :: Display
    , _root               :: !Window
    , _userConf           :: !UserConf
    , _normalBorderColor  :: !Pixel
    , _focusedBorderColor :: !Pixel
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
    }

type XMColor = (String, String)

data XMobarConf = XMobarConf
    { _currentC     :: !XMColor
    , _hiddenC      :: !XMColor
    , _hiddenEmptyC :: !XMColor
    , _titleC       :: !XMColor
    , _handle       :: !Handle 
    } deriving (Eq)

$(mkLabels [''SUNConf, ''UserConf, ''XMobarConf])

liftX :: MonadIO m => IO a -> m a
liftX = liftIO

spawnTerminal :: SUN ()
spawnTerminal = asks (terminal . userConf) >>= spawn

getColor :: Display -> ScreenNumber -> String -> IO Pixel
getColor dis scr str = let cMap = defaultColormap dis scr in
    fmap (color_pixel . fst) (allocNamedColor dis cMap str)

-- | Does basic Xlib setup and generates a SUNConf object based on 
-- user-specified settings in a UserConf.
setup :: UserConf -> IO SUNConf
setup !uc = openDisplay [] >>= \dis -> do
    installSignalHandlers
    let scr = defaultScreen dis
        rt  = defaultRootWindow dis
    fc <- getColor dis scr $ L.get focusedBorder uc
    nc <- getColor dis scr $ L.get normalBorder uc
    ms <- getModifierMapping dis 
    nmlck <- fmap (foldr (.|.) 0) $ sequence [ do
            ks <- keycodeToKeysym dis kc 0
            if ks == xK_Num_Lock
            then return (setBit 0 (fromIntegral m)) 
            else return (0 :: KeyMask)
            | (m, kcs) <- ms, kc <- kcs, kc /= 0]
    selectInput dis rt $  substructureRedirectMask .|. substructureNotifyMask .|. structureNotifyMask
                          .|. buttonPressMask .|. propertyChangeMask
    liftX $ ungrabButton dis anyButton anyModifier rt >> ungrabKey dis anyKey anyModifier rt
    xSetErrorHandler
    hSetBuffering stdout NoBuffering
    sync dis False
    return $ SUNConf dis rt uc nc fc nmlck

splitH r = safeModify focusWS (doSplit $ hSplit r) >>
           arrange >> refresh >> updateFocus >> updateBar

splitV r = safeModify focusWS (doSplit $ vSplit r) >>
           arrange >> refresh >> updateFocus >> updateBar

-- | Configure the sizes of all mapped windows according to current tree shape. 
arrange :: SUN ()
arrange = asks display >>= \dis -> do
    configureBarScr
    t <- gets (tree . focusWS)
    sw <- gets screenWidth ; sh <- gets screenHeight
    bw <- fmap ((*) 2) $ asks (borderWidth . userConf)
    bh <- fmap fip $ gets barHeight
    let ws = flattenToDimWins sw sh t
        putWindow ((x,y,w,h),win) = moveResizeWindow dis win x (y+bh) (w-bw) (h-bw)
    when (length ws > 1 || L.get trail t /= []) $ liftX $ mapM_ putWindow ws
    when (length ws == 1 && L.get trail t == []) $ liftX 
         $ moveResizeWindow dis (snd $ head ws) 0 bh sw sh

resizeFrame :: Direction -> SplitRatio -> SUN ()
resizeFrame dir dr = gets (trail . tree . focusWS) >>= \t ->
  when (t /= []) $ do
    safeModify (tree . focusWS) (resize dir dr) 
    arrange >> refresh >> updateFocus >> updateBar

safeModify :: (SUNState :-> a) -> (a -> a) -> SUN ()
safeModify lens f = do
    ss' <- stateFunc (L.modify lens f)
    sw <- gets screenWidth ; sh <- gets screenHeight ; let nt = L.get (tree . focusWS) ss'
    inFS <- gets inFullScreen
    when (not inFS && (all notTooSmall $ map fst $ flatten nt sw sh)) $ lens =. f
  where notTooSmall (_,_,w,h) = w > 50 && h > 50

-- | Cycles through hidden windows in given direction.
raiseHidden :: Direction -> SUN ()
raiseHidden dir = do
  ff <- fmap isJust $ gets (focusFloat . focusWS)
  when (not ff) $ do
    focusWS =. (cycleHidden dir)
    arrange >> refresh >> updateFocus >> updateBar
 
-- | Used for when you want to apply a function to the whole state at once.
-- i.e. function with type signature (SUNState -> SUNState)
modifyState :: MonadState s m => (s -> s) -> m ()
modifyState ssf = Control.Monad.State.get >>= \ss -> put (ssf ss)

stateFunc ::  MonadState a m => (a -> b) -> m b
stateFunc ssf = Control.Monad.State.get >>= \ss -> return (ssf ss)

-- | Moves the currently focused window to another workspace.
moveWinToWS :: Int -> SUN ()
moveWinToWS wsn = modifyState (moveToWS wsn) >> arrange >> refresh >> updateBar >> updateFocus

-- | Map all windows that should be visible, unmap all window that shouldn't.
refresh :: SUN ()
refresh = asks display >>= \dis -> do
    modifyState updateWorkspace
    vws <- fmap getVisWins $ gets focusWS ; aws <- stateFunc getAllWins
    fs <- gets (floats . focusWS) ; afs <- fmap (map (L.get floats)) $ gets workspaces
    liftX $ mapM_ (mapWindow dis) $ vws ++ fs ; liftX $ mapM_ (unmapWindow dis) $ (aws \\ vws) ++ (concat afs \\ fs)

-- | Switch to another workspace
changeWS :: Int -> SUN ()
changeWS wsn = gets focusWSNum >>= \fwsn -> do
    when (fwsn /= wsn) $ modifyState (changeWorkspace wsn)
    refresh >> arrange >> updateBar >> updateFocus

runSUN :: SUN a -> SUNState -> SUNConf -> IO a
runSUN (SUN a) !st !c = evalStateT (runReaderT a c) st

-- | Move mouse to far bottom right corner of screen.
banish :: SUN ()
banish = do
    dis <- asks display ; rt <- asks root ; bh <- fmap fip $ gets barHeight
    sw <- gets screenWidth ; sh <- gets screenHeight
    liftX $ warpPointer dis none rt 0 0 0 0 (fi sw) $ fi sh + bh

equalize = safeModify (tree . focusWS) makeEqual >> arrange

flipT = safeModify (tree . focusWS) flipTree >> arrange >> refresh >> updateFocus

-- | Kill a window. Properly. Thanks XMonad!
killWindow :: SUN ()
killWindow = asks display >>= \dis -> do
    wmdelt <- atom_WM_DELETE_WINDOW
    wmprot <- atom_WM_PROTOCOLS
    fw <- fmap focusedWin $ gets focusWS
    ffw <- gets (focusFloat . focusWS)
    liftX $ when (isJust fw || isJust ffw) $ do
    let w = if (isJust ffw) then fromJust ffw else fromJust fw
    protocols <- getWMProtocols dis w
    if wmdelt `elem` protocols
      then allocaXEvent $ \ev -> do
           setEventType ev clientMessage
           setClientMessageEvent ev w wmprot 32 wmdelt 0
           sendEvent dis w False noEventMask ev
      else killClient dis w >> return ()

removeWindow :: Window -> SUN ()
removeWindow w = do
  fs <- gets (floats . focusWS)
  ff <- gets (focusFloat . focusWS)
  fw <- fmap focusedWin $ gets focusWS
  if (isJust ff)
  then let ffw = fromJust ff in
    when (ffw == w) $ (focusFloat . focusWS) =: Nothing
  else do 
    modifyState (annihilateWin w)
    case fw of
      Just w' -> when (w' == w) $ do
        raiseHidden R
        fw' <- fmap focusedWin $ gets focusWS
        when ((not $ isJust fw') && (not $ null fs))
            $ (focusFloat . focusWS) =: (Just $ head fs) 
      Nothing -> return ()
  (floats . focusWS) =. (delete w)
  arrange >> refresh >> updateBar >> updateFocus

-- | Wrapper for the common case of atom internment
getAtom :: String -> SUN Atom
getAtom str = asks display >>= \dis -> liftX $ internAtom dis str False

isInProperty :: String -> String -> Window -> SUN Bool
isInProperty p v w = do
    va <- getAtom v
    r <- getProp32s p w
    return $ case r of
        Just xs -> fromIntegral va `elem` xs
        _ -> False

-- | Returns true if the window labels itself as being fullscreen.
isFullscreen :: Window -> SUN Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
isDock = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

-- | Get a window property from atom
getProp32 :: Atom -> Window -> SUN (Maybe [CLong])
getProp32 a w = asks display >>= \dis -> liftX $ getWindowProperty32 dis a w

-- | Get a window property from string
getProp32s :: String -> Window -> SUN (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }

-- | Common non-predefined atoms
atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_STATE :: SUN Atom
atom_WM_PROTOCOLS       = getAtom "WM_PROTOCOLS"
atom_WM_DELETE_WINDOW   = getAtom "WM_DELETE_WINDOW"
atom_WM_STATE           = getAtom "WM_STATE"

configureBarScr :: SUN ()
configureBarScr = do
  dis <- asks display ; rt <- asks root
  (_,_,qt) <- liftX $ queryTree dis rt
  qt' <- filterM isDock qt
  let scr = defaultScreen dis
      sw = fid (displayWidth dis scr)
      sh = fid (displayHeight dis scr)
  when (not $ null qt') $ do
      bwa <- liftX $ getWindowAttributes dis (head qt')
      let bh = fid $ wa_height bwa + wa_y bwa
      barHeight =: bh
      screenHeight =: sh - bh
  when (null qt) $ do
      barHeight =: 0
      screenHeight =: sh
      screenWidth =: sw

-- | Sends formated workspace boxes and window names to stdin of xmobar. 
updateBar :: SUN ()
updateBar = asks (barConf . userConf) >>= \c -> do
    modifyState updateWorkspace ; dis <- asks display
    wsns <- asks (wsNames . userConf) ; fw <- fmap focusedWin $ gets focusWS
    ffw <- gets (focusFloat . focusWS)
    fwsn <- gets focusWSNum ; wss <- gets workspaces
    wTitle <- liftX $ maybe (return Nothing) (fetchName dis) fw
    fwTitle <- liftX $ maybe (return Nothing) (fetchName dis) ffw
    let h = L.get handle c ; cur = L.get currentC c ; hid = L.get hiddenC c 
        hidE = L.get hiddenEmptyC c ; titC = L.get titleC c
        wCount ws = length $ (getHidWins ws) ++ (getVisWins ws) ++ (L.get floats ws)
        xmb (fgc,bgc) str = 
          "<fc=" ++ fgc ++ "," ++ bgc ++ ">" ++ " " ++ take 45 str ++ " " ++ "</fc>"
        wsF (ws,n)
            | n == (fwsn-1) = xmb cur (wsns !! n)
            | wCount ws == 0 = xmb hidE (wsns !! n)
            | otherwise = xmb hid (wsns !! n)
        frmtns = concatMap wsF $ zip wss [0..]
        wTitle' = if (isJust ffw && isJust fwTitle) then (xmb titC $ fromJust fwTitle)
                  else (maybe "" (xmb titC) wTitle)
    vwins <- (fmap getVisWins $ gets focusWS) >>= \vws -> mapM (liftX . fetchName dis)
             $ if ((not $ isJust ffw) && (isJust fw)) then delete (fromJust fw) vws else vws
    hwins <- (fmap getHidWins $ gets focusWS) >>= mapM (liftX . fetchName dis)
    fwins <- (gets (floats . focusWS)) >>= \fws -> mapM (liftX . fetchName dis)
             $ if (isJust ffw) then delete (fromJust ffw) fws else fws
    let vs = map (xmb hid . fromJust) $ filter (/= Nothing) $ vwins ++ fwins
    let hs = map (xmb hidE . fromJust) $ filter (/= Nothing) hwins
    liftX $ hPutStrLn h $ filter (`elem` ['\32'..'\126']) $ frmtns 
            ++ " " ++ wTitle' ++ " " ++ intercalate "|" (vs ++ hs)

-- | Update the focused window, redraw borders accordingly on all mapped windows.
updateFocus :: SUN ()
updateFocus = do
    (dis,rt,sw,sh,nc,fc,uc) <- getConf
    ws <- fmap getVisWins $ gets focusWS
    fw <- fmap focusedWin $ gets focusWS
    ff <- gets (focusFloat . focusWS)
    fs <- gets (floats . focusWS)
    tl <- gets (trail . tree . focusWS)
    bh <- gets barHeight
    let unFocus win = setWindowBorder dis win nc >> setWindowBorderWidth dis win bw
        bw = L.get borderWidth uc
    liftX $ clearWindow dis rt
    case ff of 
      (Just ffw) -> do
        inFS <- gets inFullScreen
        xGrabButton False ffw
        if inFS 
          then liftX $ do
            setWindowBorderWidth dis (ffw) 0
            moveResizeWindow dis (ffw) 0 0 sw (sh+bh)
          else liftX $ do
            setWindowBorderWidth dis ffw bw
            setWindowBorder dis ffw fc
        mapM_ (xGrabButton True) $ ws ++ (delete ffw fs)
        if (tl == [] && ws /= [])
          then liftX $ setWindowBorderWidth dis (head ws) 0
          else liftX $ mapM_ unFocus $ ws ++ (delete ffw fs)
        liftX $ setInputFocus dis ffw revertToParent 0
        liftX $ raiseWindow dis ffw 
      Nothing -> do
        liftX $ mapM_ (raiseWindow dis) fs
        case fw of
          Just w -> do
            if tl == []
              then liftX $ setWindowBorderWidth dis w 0
              else do
                liftX $ setWindowBorderWidth dis w bw
                liftX $ setWindowBorder dis w fc
            liftX $ setInputFocus dis w revertToParent 0
            mapM_ (xGrabButton True) $ (delete w ws) ++ fs
            xGrabButton False w
            liftX $ mapM_ unFocus $ (delete w ws ++ fs)
          Nothing -> do
            liftX $ setInputFocus dis rt revertToParent 0
            when (tl /= []) drawFrameBorder
            liftX $ mapM_ unFocus $ ws ++ fs
            mapM_ (xGrabButton True) $ ws ++ fs

xGrabButton :: Bool -> Window -> SUN ()                                                    
xGrabButton grab w = asks display >>= \dis -> do
    nmlck <- asks numlockMask
    liftX $ if grab
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
       
-- | Draws the dashed border around a focused, empty frame.
drawFrameBorder :: SUN ()
drawFrameBorder = do
    (dis,rt,sw,sh,nc,fc,uc) <- getConf
    t <- gets (tree . focusWS)
    fs <- gets (floats . focusWS)
    bh <- gets barHeight
    let mt = find ((==) t . snd) $ flatten t sw sh
        bw = L.get borderWidth uc
    when (isJust mt && null fs) $ liftX $ do
        let ((x,y,w,h),nt) = fromJust mt
        gc <- createGC dis rt
        setLineAttributes dis gc (fi bw) lineSolid capButt joinMiter
        setForeground dis gc fc
        drawRectangle dis rt gc (fi x + (div (fip bw) 2)) (fi y + (div (fip bw) 2) + (fip bh)) (w-(fid bw)) (h-(fid bw))
        sync dis False
        freeGC dis gc

getConf :: SUN (Display, Window, Dimension, Dimension, Pixel, Pixel, UserConf)
getConf = do
    dis <- asks display ; fc <- asks focusedBorderColor ; uc <- asks userConf
    nc <- asks normalBorderColor ; sw <- gets screenWidth
    sh <- gets screenHeight ; rt <- asks root
    return (dis,rt,sw,sh,nc,fc,uc)

eventDispatch :: Event -> SUN ()

eventDispatch !evt@(UnmapEvent {ev_window = w, ev_send_event = synthetic}) = do
  when synthetic (removeWindow w)

eventDispatch !(MapRequestEvent {ev_window = win}) = do
    (dis,rt,sw,sh,nc,fc,uc) <- getConf
    fw <- fmap focusedWin $ gets focusWS
    allWins <- stateFunc getAllWins
    isF <- isFullscreen win ; isD <- isDialog win ; isS <- isSplash win
    ws <- gets focusWS
    unless (win `elem` allWins || isF || isD || isS) $ do
      when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
      (tree . focusWS) =. (replace (Just win))
      (focusFloat . focusWS) =: Nothing
      arrange >> refresh >> updateFocus >> updateBar
    when (isF || isS || isD) $ float win

eventDispatch !evt@(ButtonEvent {ev_window = w, ev_event_type = t, ev_x = x, ev_y = y})
  | t == buttonPress = do
      rt <- asks root
      when (w == rt) $ clickFocusEmptyFrame (fid x, fid y)
      when (w /= rt) $ do
      dis <- asks display
      cm <- cleanMask $ ev_state evt
      when ((cm, ev_button evt) == (0,button1)) $ clickFocus w
      when ((cm, ev_button evt) == (mod1Mask,button1)) $ mouseMove w
      when ((cm, ev_button evt) == (mod1Mask,button3)) $ mouseResize w

eventDispatch !evt@(MotionEvent {ev_event_type = _t, ev_x = x, ev_y = y}) = do
  drag <- gets dragging
  dis <- asks display
  case drag of
    (Just (Move win wax way pox poy)) ->
      liftX $ moveWindow dis win (wax + (fip x - pox)) (way + (fip y - poy))
    (Just (Resize win w h)) ->
      liftX $ resizeWindow dis win (fid x - w) (fid y - h)

    Nothing -> return () -- This shouldn't ever happen though. 

eventDispatch !evt@(ButtonEvent {ev_window = w, ev_event_type = t})
  | t == buttonRelease = do
      drag <- gets dragging
      case drag of
        (Just _) -> do
          dis <- asks display
          liftX $ ungrabPointer dis currentTime
          dragging =: Nothing
        Nothing -> return ()


eventDispatch !(DestroyWindowEvent {ev_window = w}) = removeWindow w

eventDispatch !evt@(ConfigureRequestEvent _ _ _ dis _ win x y w h bw a d vm) = do
    ws <- fmap getVisWins $ gets focusWS
    wa <- liftX $ getWindowAttributes dis win
    if win `notElem` ws
        then liftX $ configureWindow dis win vm $ WindowChanges x y w h bw a d
        else liftX $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev win win
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width evt) none 
                     (wa_override_redirect wa)
                 sendEvent dis win False 0 ev
    liftX $ sync dis False
    arrange >> updateFocus >> updateBar

eventDispatch !evt@(KeyEvent {ev_event_type = et}) = when (et == keyPress) $ do
    dis <- asks display ; rt <- asks root ; inP <- gets inPrefix ; p <- asks (prefixKey . userConf)
    let km = ev_state evt ; kc = ev_keycode evt ; makeCursor c = liftX $ createFontCursor dis c
    ks <- liftX $ keycodeToKeysym dis kc 0
    when ((km,ks) == p && not inP) $ do
      inPrefix =: True
      cur <- makeCursor xC_cross_reverse
      liftX $ grabPointer dis rt False 0 grabModeAsync grabModeAsync none cur currentTime
      liftX $ grabKeyboard dis rt True grabModeAsync grabModeAsync currentTime
      liftX $ freeCursor dis cur
    when inP $ do
      kbs <- asks (keyBinds . userConf)
      let kt = M.lookup (km,ks) kbs
      case kt of
          (Just act) -> act >> do
            liftX $ ungrabKeyboard dis currentTime
            liftX $ ungrabPointer dis currentTime
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

-- | Grab prefix key and top-level bindings.
grabPrefixTops :: SUN ()
grabPrefixTops = do
    dis <- asks display
    rt <- asks root
    keys <- fmap M.keys $ asks (topKeyBinds . userConf)
    liftX $ ungrabKey dis anyKey anyModifier rt
    let xGrabKey (km,kc) = grabKey dis kc km rt True grabModeSync grabModeAsync
        toKeyCode (km,ks) = do
            kc <- keysymToKeycode dis ks
            return (km,kc)
    keys' <- liftX $ mapM toKeyCode keys
    p <- asks (prefixKey . userConf)
    p' <- liftX $ toKeyCode p
    liftX $ xGrabKey p' >> mapM_ xGrabKey keys'

quit :: SUN ()
quit = liftX exitSuccess

-- | Swap the content (empty or not) of two adjacent frames in specified direction.
swap :: Direction -> SUN ()
swap dir = do
    t <- gets (tree . focusWS)
    sw <- gets screenWidth ; sh <- gets screenHeight
    let fw = fromFrame t ; nw = fromFrame $ changeFocus dir sw sh t
    when (nw /= fw) $ do
        safeModify (tree . focusWS) (replace fw . changeFocus dir sw sh . replace nw)
        arrange >> updateFocus >> updateBar

removeFrame :: SUN ()
removeFrame = do
    ffw <- gets (focusFloat . focusWS)
    when (not $ isJust ffw) $ do
      safeModify focusWS killFrame
      arrange >> refresh >> updateFocus >> updateBar

focusTo :: Direction -> SUN ()
focusTo dir = do
    ffw <- gets (focusFloat . focusWS)
    when (not $ isJust ffw) $ do
      sw <- gets screenWidth ; sh <- gets screenHeight
      safeModify (tree . focusWS) $ changeFocus dir sw sh
      arrange >> refresh >> updateFocus >> updateBar

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

-- | Entry point.
sunwm :: UserConf -> IO ()
sunwm !uc = setup uc >>= runSUN 
    (grabPrefixTops >> updateBar >> configureBarScr >> eventLoop) st
  where st = initState $ length $ L.get wsNames uc

-- | Core function of the whole window manager.  Recieves the events
-- and sends them to eventDispatch.
eventLoop :: SUN ()
eventLoop = do
    dis <- asks display
    evt <- liftX $ do
      sync dis False
      allocaXEvent $ \evtpnt -> do
      nextEvent dis evtpnt
      getEvent evtpnt
    eventDispatch evt
    eventLoop

setMouseDrag :: (Maybe DragType) -> SUN ()
setMouseDrag dragType = do
    rt <- asks root ; dis <- asks display
    liftX $ grabPointer dis rt False (buttonReleaseMask .|. pointerMotionMask)        
            grabModeAsync grabModeAsync none none currentTime                      
    dragging =: dragType

-- | Strip numlock && capslock from a mask
cleanMask :: KeyMask -> SUN KeyMask
cleanMask km = asks numlockMask >>= \nml ->
    return (complement (nml .|. lockMask) .&. km)

mouseMove :: Window -> SUN ()
mouseMove w = asks display >>= \dis -> do
  float w
  wa <- liftX $ getWindowAttributes dis w
  let x = wa_x wa ; y = wa_y wa
  (_,_,_,pox,poy,_,_,_) <- liftX $ queryPointer dis w
  setMouseDrag $ Just (Move w (fip x) (fip y) (fip pox) (fip poy))

mouseResize :: Window -> SUN ()
mouseResize w = asks display >>= \dis -> do
  float w
  wa <- liftX $ getWindowAttributes dis w
  liftX $ warpPointer dis none w 0 0 0 0 (fip (wa_width wa)) (fip (wa_height wa))
  setMouseDrag $ Just (Resize w (fid $ wa_x wa) (fid $ wa_y wa))

clickFocus :: Window -> SUN ()
clickFocus w = do
  tr <- gets (tree . focusWS)
  fs <- gets (floats . focusWS)
  vws <- fmap getVisWins $ gets focusWS
  when (w `elem` fs) $ do
    (focusFloat . focusWS) =: (Just w)
  when (w `elem` vws) $ do
    let zs = filter ((/=) Nothing . fromFrame) $ flattenToZips tr
        (Just cwz) = find ((==) (Just w) . fromFrame) zs
    (tree . focusWS) =: cwz
    (focusFloat . focusWS) =: Nothing
  updateFocus >> updateBar

float :: Window -> SUN ()
float w = do
  modifyState (annihilateWin w)
  (focusFloat . focusWS) =: Just w
  fs <- gets (floats . focusWS) 
  unless (w `elem` fs) $ do
    (floats . focusWS) =. (w:)
    dialog <- isDialog w
    when (not dialog) $ focusWS =. (cycleHidden R)
  arrange >> refresh >> updateFocus >> updateBar

unfloat :: SUN ()
unfloat = do
  ffw <- gets (focusFloat . focusWS)
  when (isJust ffw) $ do
    fw <- fmap focusedWin $ gets focusWS
    when (isJust fw) $ (hidden . focusWS) =. (fromJust fw:)
    (tree . focusWS) =. (replace ffw)  
    (floats . focusWS) =. (Data.List.delete (fromJust ffw))
    (focusFloat . focusWS) =: Nothing
    arrange >> refresh >> updateFocus >> updateBar

clickFocusEmptyFrame :: (Dimension, Dimension) -> SUN ()
clickFocusEmptyFrame (x,y) = do
  sw <- gets screenWidth ; sh <- gets screenHeight    
  t <- gets (tree . focusWS)
  let zs = filter ((==) Nothing . fromFrame . snd) $ flatten t sw sh
      cwz = find (isInRectangle (x,y) . fst) zs
  when (isJust cwz) $ (tree . focusWS) =: (snd $ fromJust cwz)
  arrange >> refresh >> updateFocus >> updateBar
 where isInRectangle (x,y) (rx,ry,rw,rh) = 
        (x > rx) && (x < (rx+rw)) && (y > ry) && (y < (ry + rh))

toggleFullScreen :: SUN ()
toggleFullScreen = do
  (dis,_,sw,sh,_,_,uc) <- getConf
  ffw <- gets (focusFloat . focusWS)
  fw <- fmap focusedWin $ gets focusWS
  when ((not $ isJust ffw) && isJust fw) $ do
    sw <- gets screenWidth ; sh <- gets screenHeight
    float $ fromJust fw
    inFullScreen =: True
    let bw = L.get borderWidth uc
    arrange >> refresh >> updateFocus >> updateBar
  when (isJust ffw) $ do
    unfloat
    inFullScreen =: False
    arrange >> refresh >> updateFocus >> updateBar
