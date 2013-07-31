{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, TypeOperators #-}
module Sunwm.Extra.Bars.Util where

import Prelude hiding ((.), id)
import Control.Category ((.))
import Data.Label.PureM (gets,asks)
import qualified Data.Label as L
import Data.Maybe (catMaybes)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Window)
import Control.Applicative
import System.Process
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import System.IO

import Control.Arrow (first, second, (&&&))
import Sunwm.STree
import Sunwm.Core
import Sunwm.FocusMap

data BarConf = BarConf
    { _focusColor       :: !(String, String)
    , _hiddenColor      :: !(String, String)
    , _hiddenEmptyColor :: !(String, String)
    , _titleColor       :: !(String, String)
    } deriving (Show, Eq)

$(L.mkLabels [''BarConf])

putStrBar :: Handle -> String -> SUN ()
putStrBar h str = liftIO $ hPutStrLn h str >> hFlush h

getWinTitles :: [Window] -> SUN [String]
getWinTitles !wins = asks display >>= \dis -> catMaybes <$> ioMap (fetchName dis) wins

-- updateBarN :: ((Int,Handle) -> SUN ()) -> SUN ()
-- updateBarN updater = do
--     screenNumList <- keys <$> gets screens
--     screenHandles <- mapF (L.get barHandle) <$> gets screens
--     return ()
--     --mapM_ updater $ zip screenNumList screenHandles
