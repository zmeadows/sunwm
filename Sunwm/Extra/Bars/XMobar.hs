{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}

module Sunwm.Extra.Bars.XMobar where

import Sunwm.Core
import Sunwm.FocusMap
import Sunwm.STree

import Prelude hiding ((.), id)
import Control.Category ((.))

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Data.Label.PureM (asks, gets, (=:))
import qualified Data.Label as L
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (delete, intercalate)

import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Window)

import System.Process
import System.IO

data BarConf = BarConf
    { _focusColor       :: !(String, String)
    , _hiddenColor      :: !(String, String)
    , _hiddenEmptyColor :: !(String, String)
    , _titleColor       :: !(String, String)
    } deriving (Show, Eq)

$(L.mkLabels [''BarConf])

xmobar :: BarConf -> UserConf -> UserConf
xmobar bc uc =
    L.modify initHook (>> initXMobar)
    $ L.modify stackHook (>> updateXMobars bc) uc

xmobarRunner :: Int -> SUN ()
xmobarRunner n = do
    let getHandle (h,_,_,_) = h
    hs <- liftIO $ runInteractiveCommand $ "xmobar -x " ++ show (n - 1)
    (barHandle . screenN n) =: Just (getHandle hs)

initXMobar :: SUN ()
initXMobar = do
    screenNumList <- keys <$> gets screens
    mapM_ xmobarRunner screenNumList


updateXMobars :: BarConf -> SUN ()
updateXMobars bc = do
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

        let wsFormat (sn,ws)
                | sn == (fwsn-1) = formatStrXMobar focC (wsns !! sn)
                | not (null (getWSAllWins ws)) = formatStrXMobar hidC (wsns !! sn)
                | otherwise      = formatStrXMobar ehidC (wsns !! sn)
            fwsns = concatMap wsFormat $ zip [0..] wss
            isChar c = c `elem` ['\32'..'\126']

        visWinTitles <- map (formatStrXMobar hidC) <$> getWinTitles (maybe vs (delete `flip` vs) fw)
        hidWinTitles <- map (formatStrXMobar ehidC) <$> getWinTitles hs
        focusTitle <- fmap (maybe "" (formatStrXMobar titC)) $ liftIO $ maybe (return Nothing) (fetchName dis) fw
        putStrBar (fromJust h) $ filter isChar $ fwsns ++ " " ++ focusTitle ++ intercalate "|" (visWinTitles ++ hidWinTitles)

formatStrXMobar :: (String,String) -> String -> String
formatStrXMobar (fg,bg) !str = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ " " ++ str ++ " " ++ "</fc>"

getWinTitles :: [Window] -> SUN [String]
getWinTitles !wins = asks display >>= \dis -> catMaybes <$> ioMap (fetchName dis) wins

putStrBar :: Handle -> String -> SUN ()
putStrBar h str = liftIO $ hPutStrLn h str >> hFlush h

