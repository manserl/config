import System.Exit
import Data.Ratio ((%))
import Array
import qualified Data.Map

import Prelude hiding( mod )

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as S
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Util.Run 
import XMonad.Actions.UpdatePointer

-- overall Config

myTerminal   = "urxvt +sb -fn '-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*' -bg black -fg '#00aa00'"
myBrowser    = "chromium"
myLauncher   = "gmrun"
myWorkspaces = ["q:main","w:W","e:E","r:R","u:U","i:I","o:O","p:P"]  -- format "keybinding:ws-name"
modifierMask = mod4Mask


-- Workspace Colors 

fgColorCurrent  = "#ffffff"
bgColorCurrent  = "#ff4000"
fgColorVisible  = "#303030"
bgColorVisible  = "#757575"
fgColorHidden   = "#ffffff"
bgColorHidden   = ""
fgColorEmpty    = "#555555"
bgColorEmpty    = ""

tabbedTheme = Theme {
    activeColor         = bgColorCurrent,
    inactiveColor       = bgColorHidden,
    urgentColor         = "#ff0000",
    activeBorderColor   = bgColorCurrent,
    inactiveBorderColor = bgColorHidden,
    urgentBorderColor   = "#ff0000",
    activeTextColor     = fgColorCurrent,
    inactiveTextColor   = fgColorHidden,
    urgentTextColor     = fgColorHidden,
    fontName            = "Snap",
    decoWidth           = 0,
    decoHeight          = 10
  }

-- Main

main = xmonad defaultConfig {
    manageHook          = myHooks,
    logHook             = myLogHook >> updatePointer (Relative 0.5 0.5),
    layoutHook          = myLayout,
    modMask             = modifierMask,
    keys                = myKeys,
    workspaces          = myWorkspaces,
    startupHook         = setWMName "LG3D",
    focusedBorderColor  = bgColorCurrent,
    normalBorderColor   = "#000000"
  }

myHooks = composeOne [
    isVideo      -?> doFloat,
    isFullscreen -?> doFullFloat,
    isDialog     -?> doCenterFloat
  ]
  where
    isVideo = className =? "MPlayer"


-- Layout

myLayout = (
            smartBorders $
            avoidStruts $
            reflectHoriz $ withIM (1%4) gimpTools $ reflectHoriz $
            addTabs shrinkText tabbedTheme $
            subLayout [] Simplest mainLayout
           )
           ||| noBorders Full
  where
    mainLayout   = (smartBorders . boringWindows) tiled ||| Full
    tiled        = Tall 1 delta masterRatio
    delta        = 3/100
    masterRatio  = 2/3
    gimpTools    = Title "Toolbox"

-- Workspace formating

myLogHook = dynamicLogWithPP $ defaultPP {
    ppTitle = shorten 60,
    ppCurrent = dzenColor fgColorCurrent bgColorCurrent . wsWithImage "/home/lukas/.dzen/corner.xbm",
    ppVisible = dzenColor fgColorVisible bgColorVisible . wsWithImage "/home/lukas/.dzen/corner.xbm",
    ppHidden  = dzenColor fgColorHidden  bgColorHidden  . wsWithImage "/home/lukas/.dzen/corner.xbm",
    ppHiddenNoWindows = dzenColor fgColorEmpty bgColorEmpty . ws,
    ppSep     = "  ",
    ppOrder   = \(workspaces: layout: title:_) -> [workspaces, title]
  }
  where
		ws :: WorkspaceId -> String
		ws = \(key:l:ws) -> " " ++ ws ++ " "
		wsWithImage :: String -> WorkspaceId -> String
		wsWithImage = \img (key:l:ws) -> "^i(" ++ img  ++ ")" ++ ws ++ " "


-- modifier functions
mod      = \key -> (modifierMask, key)
ctrl     = \key -> (controlMask, key)
modShift = \key -> (modifierMask .|. shiftMask, key)
alt      = \key -> (mod1Mask, key)
noMod    = \key -> (0, key)

-- custom keys
xK_AudioMute        = 0x1008ff12;
xK_AudioLowerVolume = 0x1008ff11;
xK_AudioRaiseVolume = 0x1008ff13;
xK_Sleep            = 0x1008ff10;

-- Array to transfer a Char to a keySym
keyCodes = array ('!','~') (zip ['!' .. '~'] [xK_exclam .. xK_asciitilde])

-- convert workspace names into keySyms
wssToKeys :: [String] -> [KeySym]
wssToKeys = \wss -> map (\(c:s) -> keyCodes!c) wss

mergeNeigh :: Direction1D -> Window -> GroupMsg Window
mergeNeigh d w = WithGroup g w
  where g cs = do
          stack <- currentStack
          whenJust stack $ sendMessage . XMonad.Layout.SubLayouts.Merge (S.focus cs) . neighGroup d . trimStack
          return $ S.Stack (S.focus cs) [] []
          where
            trimStack s  = S.Stack (S.focus s) (onlyOthers . S.up $ s) (onlyOthers . S.down $ s)
            onlyOthers   = filter (`notElem` S.integrate cs)
            currentStack = gets (S.stack . S.workspace . S.current . windowset)
{-
mergeNeigh :: Direction1D -> X ()
mergeNeigh d =
  where g cs = do
          stack <- currentStack
          whenJust stack $ sendMessage . XMonad.Layout.SubLayouts.Merge (S.focus cs) . neighGroup d . trimStack
          return cs
          where
            trimStack s  = S.Stack (S.focus s) (onlyOthers . S.up $ s) (onlyOthers . S.down $ s)
            onlyOthers   = filter (`notElem` S.integrate cs)
            currentStack = gets (S.stack . S.workspace . S.current . windowset)
-}


neighGroup :: Direction1D -> S.Stack a -> a
neighGroup Next s =
  case (S.up s, S.down s) of
    (xs, []) -> last xs
    (_, x:_) -> x

neighGroup Prev s =
  case (S.up s, S.down s) of
    ([], xs) -> last xs
    (x:_, _) -> x

        
-- keybindings

myKeys conf@(XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $ [
    -- launch programs
    (mod xK_t,          spawn (myTerminal ++ " >/dev/null")),
    (mod xK_z,          spawn (myBrowser ++ " >/dev/null")),
    (mod xK_backslash,  spawn (myLauncher ++ " >/dev/null")),

    -- window commands
    (mod      xK_c,   kill),
    (mod      xK_Tab, focusDown),
    (modShift xK_Tab, focusUp),
    (mod      xK_j,   windows S.focusDown),
    (mod      xK_k,   windows S.focusUp),
    (mod      xK_a,   windows S.swapMaster),
    (mod      xK_s,   windows S.swapDown),
    (mod      xK_d,   windows S.swapUp),
    (mod      xK_f,   withFocused $ windows . S.sink),

    (mod      xK_comma,  onGroup S.focusUp'),
    (mod      xK_period, onGroup S.focusDown'),

    (modShift xK_l, withFocused $ sendMessage . mergeNeigh Next),
    (modShift xK_h, withFocused $ sendMessage . mergeNeigh Prev),
    (modShift xK_y, withFocused $ sendMessage . UnMerge),

    -- Layout commands
    (mod      xK_space, sendMessage NextLayout),
    (modShift xK_space, setLayout $ XMonad.layoutHook conf),
    (mod      xK_h,     sendMessage Shrink),
    (mod      xK_l,     sendMessage Expand),
    (mod      xK_b,     sendMessage (IncMasterN 1)),
    (mod      xK_n,     sendMessage (IncMasterN (-1))),

    -- Audio controll
    (noMod xK_AudioMute,        spawn "~/scripts/audio.sh toggle >/dev/null"),
    (noMod xK_AudioLowerVolume, spawn "~/scripts/audio.sh unmute 5%- >/dev/null"),
    (noMod xK_AudioRaiseVolume, spawn "~/scripts/audio.sh unmute 5%+ >/dev/null"),

    -- Standby
	  (noMod xK_Sleep, spawn "sudo s2ram --force --vbe_post --vbe_mode >/dev/null"),

    -- Termination commands
    (ctrl xK_Escape, io (exitWith ExitSuccess)),
    (alt xK_Shift_L, restart "xmonad" True)
  ]
  ++

  -- mod-workspaceKey(N), Switch to workspace N
  -- mod-shift-workspaceKey(N), Move client to workspace N
  [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) (wssToKeys (XMonad.workspaces conf))
    , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]
  ]
  ++

  -- mod-{1,2,3}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{1,2,3}, Move client to screen 1, 2, or 3
  [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
    , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]
  ]
