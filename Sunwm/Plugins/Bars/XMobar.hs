{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DeriveDataTypeable, TemplateHaskell, TypeOperators #-}

module Sunwm.Plugins.Bars.XMobar where

import Sunwm.Core
import Sunwm.FocusMap
import Sunwm.STree
import Sunwm.Plugin

import Prelude hiding ((.), id)
import Control.Category ((.))

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Data.Label.PureM (asks, gets)
import qualified Data.Label as L
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (delete, intercalate)
import Data.Dynamic
import qualified Data.Map.Strict as M

import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Window)

import System.Process
import System.IO

data BarConf = BarConf
    { _focusColor       :: !(String, String)
    , _hiddenColor      :: !(String, String)
    , _hiddenEmptyColor :: !(String, String)
    , _titleColor       :: !(String, String)
    } deriving (Show, Eq, Typeable)

data XmobarState = XmobarState
    { _barConf :: !BarConf
    , _handles :: M.Map Int Handle
    } deriving (Show, Typeable)

initBarConf :: BarConf
initBarConf = BarConf {
    _focusColor       = ("#222222", "#ff0000"),
    _hiddenColor      = ("#f8f8f8", "#222222"),
    _hiddenEmptyColor = ("#8f8f8f", "#222222"),
    _titleColor       = ("#ff0000", "#222222")
    }

initXmobarState :: BarConf -> XmobarState
initXmobarState bc = XmobarState bc M.empty

typeRepXmobar :: TypeRep
typeRepXmobar = typeOf $ initXmobarState initBarConf

$(L.mkLabels [''BarConf, ''XmobarState])

xmobar :: BarConf -> Plugin
xmobar bc = Plugin (typeOf $ initXmobarState initBarConf) (toDyn $ initXmobarState bc) initXMobar updateXMobars

xmobarRunner :: Int -> SUN ()
xmobarRunner n = do
    let getHandle (stdout_handle,_,_,_) = stdout_handle
    h <- liftIO $ runInteractiveCommand $ "xmobar -x " ++ show (n - 1)
    st <- getPD typeRepXmobar :: SUN XmobarState
    putPD (L.modify handles (M.insert n (getHandle h)) st)

initXMobar :: SUN ()
initXMobar = do
    screenNumList <- keys <$> gets screens
    mapM_ xmobarRunner screenNumList

updateXMobars :: SUN ()
updateXMobars = do
    screenNumList <- keys <$> gets screens
    bc <- L.get barConf <$> getPD typeRepXmobar
    mapM_ (updateXMobarN bc) screenNumList

-- | Sends formated workspace boxes and window names to stdin of xmobar.
updateXMobarN :: BarConf -> Int -> SUN ()
updateXMobarN (BarConf focC hidC ehidC titC) n = do
    dis <- asks display
    wsns  <- asks (wsNames . userConf)
    h <- M.lookup n <$> L.get handles <$> getPD typeRepXmobar
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

