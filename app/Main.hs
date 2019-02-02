import Sunwm.Core
import Sunwm.STree

import Graphics.X11
import Data.Bits hiding (shift)
import qualified Data.Map as M

import Sunwm.Plugins.Bars.XMobar

main :: IO ()
main = sunwm defaultConfig [xmobar myBarConf] >>= print

myBarConf :: BarConf
myBarConf = BarConf {
    _focusColor       = ("#222222", "#077D8A"),
    _hiddenColor      = ("#f8f8f8", "#222222"),
    _hiddenEmptyColor = ("#8f8f8f", "#222222"),
    _titleColor       = ("#077D8A", "#222222")
    }

workspaceNames:: [String]
workspaceNames = map show ([1..9] :: [Int])
--workspaceNames = ["web","comm","code1","code2","code3","mus","misc1","misc2"]

defaultConfig :: UserConf
defaultConfig = UserConf {
    _normalBorder  = "rgb:4f/4f/4f",
    _focusedBorder = "rgb:07/7D/8a",
    _borderWidth   = 1,
    _keyBinds      = defaultKeys,
    _topKeyBinds   = defaultTopKeys,
    _wsNames       = workspaceNames,
    _prefixKey     = (0, xK_Super_L),
    _terminal      = "xfce4-terminal 2> /dev/null",
    _initHook      = return (),
    _stackHook     = return ()
    }

defaultKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultKeys = M.fromList
    [ ((0, xK_l), focusTo R)
    , ((0, xK_h), focusTo L)
    , ((0, xK_j), focusTo D)
    , ((0, xK_k), focusTo U)
    , ((0, xK_v), splitV 0.5 >> focusTo R)
    , ((shiftMask, xK_v), splitV 0.65 >> focusTo R)
    , ((mod1Mask,  xK_v), splitV 0.35 >> focusTo R >> swapToDir L)
    , ((0, xK_n), splitH 0.5 >> focusTo D)
    , ((shiftMask, xK_n), splitH 0.65 >> focusTo D)
    , ((mod1Mask,  xK_n), splitH 0.35 >> focusTo D >> swapToDir U)
    , ((shiftMask, xK_h), swapToDir L)
    , ((shiftMask, xK_l), swapToDir R)
    , ((shiftMask, xK_k), swapToDir U)
    , ((shiftMask, xK_j), swapToDir D)
    --, ((0, xK_f), toggleFullScreen)
    , ((0, xK_o), raiseHidden R)
    , ((0, xK_i), raiseHidden L)
    , ((0, xK_r), removeFrame)
    , ((0, xK_c), spawnTerminal)
    , ((0, xK_e), makeOnly)
    , ((0, xK_q), killWindow)
    , ((0, xK_b), banish)
    , ((shiftMask, xK_q), killWindow >> removeFrame)
    , ((0, xK_Escape), return ())
    , ((0, xK_equal), equalize)
    , ((0, xK_slash), flipT)
    , ((mod1Mask, xK_h), shiftTo L)
    , ((mod1Mask, xK_l), shiftTo R)
    , ((mod1Mask, xK_j), shiftTo D)
    , ((mod1Mask, xK_k), shiftTo U)
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
    ] ++ workspaceSendBinds ++ workspaceMoveBinds

workspaceMoveBinds :: [((KeyMask, KeySym),SUN ())]
workspaceMoveBinds = map (\(k,n) -> ((mod1Mask, k), changeWS n))
                     $ zip [xK_1..] [1..length workspaceNames]

workspaceSendBinds :: [((KeyMask, KeySym),SUN ())]
workspaceSendBinds = map (\(k,n) -> ((mod1Mask .|. shiftMask, k), moveWinToWS n))
                     $ zip [xK_1..] [1..length workspaceNames]

