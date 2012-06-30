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
import Data.Bits
import System.IO
import qualified Data.Map as M

myBarConf h = XMobarConf 
  ("#0c0c0c", "#c3c3a9") -- focusWS
  ("#7f7f7f", "#e6e6dc") -- hiddenWS
  ("#bfbfbf", "#e6e6dc") -- emptyHiddenWS
  ("#0c0c0c", "#e6e6dc") -- activeWinTitle
  h

main = do
    h <- spawnPipe "xmobar"
    sunwm $ defaultConfig h

defaultConfig h = UserConf
      "rgb:4f/4f/4f"     -- Normal Border Color
      "rgb:ff/66/66"     -- Focused Border Color
      1                  -- Border Width
      defaultKeys      
      defaultTopKeys
      (map show [1..9])  -- Workspace Names (list of strings)
      (0, xK_F13) -- Prefix Key
      (myBarConf h) 
      "urxvtc +sb"            -- Terminal Command

defaultKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultKeys = M.fromList
    [ ((0, xK_l), focusTo R)
    , ((0, xK_h), focusTo L)
    , ((0, xK_j), focusTo D)
    , ((0, xK_k), focusTo U)
    , ((0, xK_v), splitV 0.5 >> focusTo R)
    , ((shiftMask, xK_v), splitV 0.65 >> focusTo R)
    , ((0, xK_n), splitH 0.5 >> focusTo D)
    , ((shiftMask, xK_n), splitH 0.65 >> focusTo D)
    , ((0, xK_p),  dmenu "-*-gohufont-medium-*-*-*-14-*-*-*-*-*-*-*"
                         "#e6e6dc" "#8f8f8f" "#e6e6dc" "#0c0c0c")
    , ((shiftMask, xK_h), swap L)
    , ((shiftMask, xK_l), swap R)
    , ((shiftMask, xK_k), swap U)
    , ((shiftMask, xK_j), swap D)
    , ((0, xK_f), toggleFullScreen)
    , ((0, xK_o), raiseHidden R)
    , ((0, xK_i), raiseHidden L)
    , ((0, xK_r), removeFrame)
    , ((0, xK_c), spawnTerminal)
    , ((0, xK_q), killWindow)
    , ((0, xK_b), banish)
    , ((shiftMask, xK_q), killWindow >> removeFrame)
    , ((0, xK_Escape), return ())
    , ((0, xK_equal), equalize)
    , ((0, xK_slash), flipT)
    , ((shiftMask, xK_space), unfloat)
    ]

defaultTopKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultTopKeys = M.fromList
    [ ((mod1Mask, xK_1), changeWS 1)
    , ((mod1Mask, xK_2), changeWS 2)
    , ((mod1Mask, xK_3), changeWS 3)
    , ((mod1Mask, xK_4), changeWS 4)
    , ((mod1Mask, xK_5), changeWS 5)
    , ((mod1Mask, xK_6), changeWS 6)
    , ((mod1Mask, xK_7), changeWS 7)
    , ((mod1Mask, xK_8), changeWS 8)
    , ((mod1Mask, xK_9), changeWS 9)
    , ((mod1Mask .|. shiftMask, xK_1), moveWinToWS 1)
    , ((mod1Mask .|. shiftMask, xK_2), moveWinToWS 2)
    , ((mod1Mask .|. shiftMask, xK_3), moveWinToWS 3)
    , ((mod1Mask .|. shiftMask, xK_4), moveWinToWS 4)
    , ((mod1Mask .|. shiftMask, xK_5), moveWinToWS 5)
    , ((mod1Mask .|. shiftMask, xK_6), moveWinToWS 6)
    , ((mod1Mask .|. shiftMask, xK_7), moveWinToWS 7)
    , ((mod1Mask .|. shiftMask, xK_8), moveWinToWS 8)
    , ((mod1Mask .|. shiftMask, xK_9), moveWinToWS 9)
    , ((mod1Mask .|. shiftMask, xK_j), resizeFrame D 0.02)
    , ((mod1Mask .|. shiftMask, xK_k), resizeFrame U 0.02)
    , ((mod1Mask .|. shiftMask, xK_l), resizeFrame R 0.02)
    , ((mod1Mask .|. shiftMask, xK_h), resizeFrame L 0.02)
    , ((mod1Mask .|. shiftMask, xK_q), quit)
    ]
