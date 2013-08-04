import Sunwm.Core
import Sunwm.STree

import Graphics.X11
import Data.Bits hiding (shift)
import qualified Data.Map.Strict as M

main :: IO ()
main = sunwm defaultConfig []

workspaceNames:: [String]
workspaceNames = map show ([1..9] :: [Integer])

defaultConfig :: UserConf
defaultConfig = UserConf {
    _normalBorder  = "rgb:4f/4f/4f",
    _focusedBorder = "rgb:ff/00/00",
    _borderWidth   = 1,
    _keyBinds      = defaultKeys,
    _topKeyBinds   = defaultTopKeys,
    _wsNames       = workspaceNames,
    _prefixKey     = (controlMask, xK_t),
    _terminal      = "xterm",
    _initHook      = return (),
    _stackHook     = return ()
    }

defaultKeys :: M.Map (KeyMask, KeySym) (SUN ())
defaultKeys = M.fromList
    [ ((0, xK_l), focusTo R)
    , ((0, xK_h), focusTo L)
    , ((0, xK_j), focusTo D)
    , ((0, xK_k), focusTo U)
    , ((0, xK_v), splitV 0.5)
    , ((0, xK_n), splitH 0.5)
    , ((shiftMask, xK_h), swapToDir L)
    , ((shiftMask, xK_l), swapToDir R)
    , ((shiftMask, xK_k), swapToDir U)
    , ((shiftMask, xK_j), swapToDir D)
    , ((0, xK_o), raiseHidden R)
    , ((0, xK_i), raiseHidden L)
    , ((0, xK_r), removeFrame)
    , ((0, xK_c), spawnTerminal)
    , ((0, xK_e), makeOnly)
    , ((0, xK_q), killWindow)
    , ((0, xK_b), banish)
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
