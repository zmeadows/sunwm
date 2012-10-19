{-
  Copyright 2011-2012 Zac Meadows

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
-}
import Core
import STree
import Util

import Graphics.X11
import Data.Bits hiding (shift)
import System.IO
import qualified Data.Map as M

main :: IO ()
main = sunwm defaultConfig >>= print

myBarConf :: Maybe BarConf
myBarConf = Just BarConf {
    _focusColor       = ("#222222", "#753E80"),
    _hiddenColor      = ("#f8f8f8", "#222222"),
    _hiddenEmptyColor = ("#8f8f8f", "#222222"),
    _titleColor       = ("#f8f8f8", "#222222"),
    }

workspaceNames:: [String]
workspaceNames = map show ([1..9] :: [Int])
--workspaceNames = ["web","comm","code1","code2","code3","mus","misc1","misc2"]

defaultConfig :: UserConf
defaultConfig = UserConf {
    _normalBorder  = "rgb:4f/4f/4f",
    _focusedBorder = "rgb:75/3E/80",
    _borderWidth   = 1,
    _keyBinds      = defaultKeys,
    _topKeyBinds   = defaultTopKeys,
    _wsNames       = workspaceNames,
    _prefixKey     = (0, xK_F13),
    _barConf       = myBarConf,
    _terminal      = "urxvtc +sb"
    }

defaultKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultKeys = M.fromList
    [ ((0, xK_l), focusTo R)
    , ((0, xK_h), focusTo L)
    , ((0, xK_j), focusTo D)
    , ((0, xK_k), focusTo U)
    , ((0, xK_v), splitV 0.5 >> focusTo R)
    , ((shiftMask, xK_v), splitV 0.65 >> focusTo R)
    , ((mod1Mask,  xK_v), splitV 0.35 >> focusTo R >> swap L)
    , ((0, xK_n), splitH 0.5 >> focusTo D)
    , ((shiftMask, xK_n), splitH 0.65 >> focusTo D)
    , ((mod1Mask,  xK_n), splitH 0.35 >> focusTo D >> swap U)
    , ((0, xK_p),  dmenu "-*-tamsyn-medium-*-*-*-14-*-*-*-*-*-*-*"
                         "#222222" "#f8f8f8" "#753E80" "#222222")
    , ((shiftMask, xK_h), swap L)
    , ((shiftMask, xK_l), swap R)
    , ((shiftMask, xK_k), swap U)
    , ((shiftMask, xK_j), swap D)
    --, ((0, xK_f), toggleFullScreen)
        , ((0, xK_o), raiseHidden R)
    , ((0, xK_i), raiseHidden L)
    , ((0, xK_r), removeFrame)
    , ((0, xK_c), spawnTerminal)
    , ((0, xK_e), only)
    , ((0, xK_q), killWindow)
    , ((0, xK_b), banish)
    , ((shiftMask, xK_q), killWindow >> removeFrame)
    , ((0, xK_Escape), return ())
    , ((0, xK_equal), equalize)
    , ((0, xK_slash), flipT)
    , ((mod1Mask, xK_h), shift L)
    , ((mod1Mask, xK_l), shift R)
    , ((mod1Mask, xK_j), shift D)
    , ((mod1Mask, xK_k), shift U)
    ]

defaultTopKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultTopKeys = M.fromList $
    [ ((mod1Mask .|. shiftMask, xK_j), resizeFrame D 0.02)
    , ((mod1Mask .|. shiftMask, xK_k), resizeFrame U 0.02)
    , ((mod1Mask .|. shiftMask, xK_l), resizeFrame R 0.02)
    , ((mod1Mask .|. shiftMask, xK_h), resizeFrame L 0.02)
    , ((mod4Mask, xK_Tab), toggleScr)
    , ((mod1Mask, xK_Tab), toggleWS)
    , ((mod1Mask .|. shiftMask, xK_q), quit)
    ] ++ workspaceSendBinds ++ workspaceMoveBinds ++ screenMoveBinds

-- | TODO: make sure this doesn't try to switch to non-existant screens?
screenMoveBinds :: [((KeyMask, KeySym),SUN ())]
screenMoveBinds = map (\(k,n) -> ((mod4Mask, k), changeScr n))
                     $ zip [xK_1..] [1..2]

workspaceMoveBinds :: [((KeyMask, KeySym),SUN ())]
workspaceMoveBinds = map (\(k,n) -> ((mod1Mask, k), changeWS n))
                     $ zip [xK_1..] [1..length workspaceNames]

workspaceSendBinds :: [((KeyMask, KeySym),SUN ())]
workspaceSendBinds = map (\(k,n) -> ((mod1Mask .|. shiftMask, k), moveWinToWS n))
                     $ zip [xK_1..] [1..length workspaceNames]
