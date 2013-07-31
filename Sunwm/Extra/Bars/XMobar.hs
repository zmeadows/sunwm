{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}
module Sunwm.Extra.Bars.XMobar where
import Sunwm.Core
import Sunwm.FocusMap
import Sunwm.STree
import Sunwm.Extra.Bars.Util

import Prelude hiding ((.), id)
import Control.Category ((.))
import Data.Label.PureM (asks, gets, (=:))
import qualified Data.Label as L
import Data.Maybe (catMaybes, isJust, fromJust)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Window)
import Control.Applicative
import System.Process
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import GHC.IO.Handle (Handle, hSetBinaryMode)
import Data.List (delete, intercalate)

xmobar :: BarConf -> UserConf -> UserConf
xmobar bc uc = L.modify initHook (\ih -> ih >> initXmobar) $ L.modify stackHook (\sh -> sh >> updateXmobars bc) uc

xmobarRunner :: Int -> SUN ()
xmobarRunner n = do
    let getHandle (h,_,_,_) = h
    hs <- liftIO $ runInteractiveCommand $ "xmobar -x " ++ show (n - 1)
    (barHandle . screenN n) =: Just (getHandle hs)

initXmobar = do
    screenNumList <- keys <$> gets screens
    mapM_ xmobarRunner screenNumList


updateXmobars bc = do
    screenNumList <- keys <$> gets screens
    mapM_ (updateXMobarN bc) screenNumList


-- | Sends formated workspace boxes and window names to stdin of xmobar.
updateXMobarN :: BarConf -> Int -> SUN ()
updateXMobarN (BarConf focC hidC ehidC titC) n = do
    dis <- asks display
    wsns  <- asks (wsNames . userConf)
    h <- gets (barHandle . screenN n)
    when (isJust h) $ do
        fw <- fromFrame <$> L.get tree <$> focused <$> gets (workspaces . screenN n)
        fwsn <- fst <$> gets (workspaces . screenN n)
        wss  <- elems <$> gets (workspaces . screenN n)
        vs   <- flattenToWins <$> L.get tree <$> focused <$> gets (workspaces . screenN n)
        hs   <- L.get hidden <$> focused <$> gets (workspaces . screenN n)

        let wsFormat (n,ws)
                | n == (fwsn-1) = formatStrXMobar focC (wsns !! n)
                | length (getWSAllWins ws) > 0  = formatStrXMobar hidC (wsns !! n)
                | otherwise      = formatStrXMobar ehidC (wsns !! n)
            fwsns = concatMap wsFormat $ zip [0..] wss
            isChar c = c `elem` ['\32'..'\126']

        visWinTitles <- map (formatStrXMobar hidC) <$> getWinTitles (maybe vs (delete `flip` vs) fw)
        hidWinTitles <- map (formatStrXMobar ehidC) <$> getWinTitles hs
        focusTitle <- fmap (maybe "" (formatStrXMobar titC)) $ liftIO $ maybe (return Nothing) (fetchName dis) fw
        putStrBar (fromJust h) $ filter isChar $ fwsns ++ " " ++ focusTitle ++ intercalate "|" (visWinTitles ++ hidWinTitles)

formatStrXMobar :: (String,String) -> String -> String
formatStrXMobar (fg,bg) !str = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ " " ++ str ++ " " ++ "</fc>"

