initBars :: SUN ()
initBars = do
   barEnabled <- isJust <$> asks (barConf . userConf)
   -- TODO: check if xmobar is in PATH
   when barEnabled $ do
       liftIO $ threadDelay 400000
       screenNumList <- keys <$> gets screens
       barHandles <- ioMap (\sn -> spawnPipe $ "xmobar -x " ++ sn) $ map (\n -> show (n - 1)) screenNumList
       --  TODO: check if launch of xmobar fails
       let assocHandle (scrNum,h) = (barHandle . screenN scrNum) =: Just h
       mapM_ assocHandle $ zip screenNumList barHandles
       -- find a way to do this without threadelay
       liftIO $ threadDelay 600000
       dis <- asks display ; rt <- asks root
       -- TODO: make sure we're getting the height of xmobar and not some other dock
       (_,_,qt) <- liftIO $ queryTree dis rt
       qt' <- filterM isDock qt
       strutinfo <- mapM_ getProp32s "_NET_WM_STRUT_PARTIAL" qt'
       liftIO $ print strutinfo
       bwa <- liftIO $ getWindowAttributes dis (head qt')
       let bh = fid $ wa_height bwa + wa_y bwa
       barHeight =: bh

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

formatStrXMobar :: (String,String) -> String -> String
formatStrXMobar (fg,bg) !str =
    "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ " " ++ str ++ " " ++ "</fc>"

getWinTitles :: [Window] -> SUN [String]
getWinTitles !wins = asks display >>= \dis -> catMaybes <$> ioMap (fetchName dis) wins

data BarConf = BarConf
    { _focusColor       :: !(String, String)
    , _hiddenColor      :: !(String, String)
    , _hiddenEmptyColor :: !(String, String)
    , _titleColor       :: !(String, String)
    } deriving (Show, Eq)


