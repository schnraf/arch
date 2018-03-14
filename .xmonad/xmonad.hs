------------------------------------------------------------------------
-- .xmonad.hs
------------------------------------------------------------------------

import XMonad
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
--import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import System.Exit

-- Main process
main :: IO()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey (ewmh $ myConfig)

-- Configs
myConfig = defaultConfig { modMask = myModMask,
                          terminal = myTerminal,
                          workspaces = myWorkspaces,
                          layoutHook = myLayoutHook,
                          manageHook = myManageHook,
                          handleEventHook = myEventHook,
                          borderWidth = myBorderWidth,
                          normalBorderColor = myNormalBorderColor,
                          focusedBorderColor = myFocusedBorderColor,
                          keys = myKeys
                          }

-- Modkey
myModMask = mod4Mask
-- Terminal
myTerminal = "urxvtc"

-- Workspace icons (awesome font)
myws1 = "\xf120"
myws2 = "\xf269"
myws3 = "\xf083"
myws4 = "\xf07b"
myws5 = "\xf121"
myws6 = "\xf1b3"
myws7 = "\xf1fc"
myws8 = "\xf1be"

myWorkspaces :: [String]
myWorkspaces = [myws1, myws2, myws3, myws4, myws5, myws6 , myws7, myws8]

-- Layouts
-- No spacing
{-myLayoutHook = avoidStruts $ smartBorders (tall ||| GridRatio (4/3) ||| Full )-}
                   {-where tall = Tall 1 (3/100) (1/2) -}

-- with spacing
myLayoutHook = (spacing 10 $ avoidStruts (tall ||| Full ||| focused)) ||| smartBorders Full
                   where
                       tall = Tall 1 (3/100) (1/3)
                       focused = gaps [(L,385), (R,385),(U,10),(D,10)]
                                 $ noBorders (Full)

-- fullscreen layout (not needed with ewmh)
--myFullscreen = (fullscreenFloat . fullscreenFull) (smartBorders Full)

-- Mangehooks
myManageHook = composeAll [ isFullscreen            --> doFullFloat,
                         className =? "Firefox" --> doShift myws2,
                         className =? "qutebrowser" --> doShift myws2,
                         className =? "Thunar" --> doShift myws4,
                         className =? "Mousepad" --> doShift myws5,
                         -- manage Gimp toolbox windows
                         className =? "Gimp"  --> doShift myws7, -- may be "Gimp" or "Gimp-2.4" instead
                         (className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat,
                         className =? "Godot" --> doShift myws6,
                         className =? "Aseprite" --> doShift myws7,
                         className =? "Filezilla" --> doShift myws4,
                         className =? "Aseprite" --> doShift myws6,
                         className =? "Inkscape" --> doShift myws7,
                         className =? "libreoffice" --> doShift myws6,
                         className =? "libreoffice-startcenter" --> doShift myws6,
                         className =? "Transmission-gtk" --> doShift myws8,
                         className =? "MPlayer" --> doFloat,
                         className =? "MPlayer" --> doShift myws8,
                         className =? "mpv" --> doFloat,
                         className =? "mpv" --> doShift myws3,
                         className =? "atom" --> doShift myws4,
                         className =? "whatsapp-desktop" --> doShift myws3,
                         appName =? "lxrandr" --> doFloat,
                         manageDocks
--                         fullscreenManageHook
                       ]
                       where role = stringProperty "WM_WINDOW_ROLE"

-- Event Hooks
myEventHook = docksEventHook <+> fullscreenEventHook

-- Looks
myBorderWidth = 4
myNormalBorderColor = "#2b303b"
myFocusedBorderColor = "#bf616a"

-- Xmonbar
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#bf616a" ""
                     , ppHidden = xmobarColor "#c0c5ce" ""
                     , ppHiddenNoWindows = xmobarColor "#4f5b66" ""
                     , ppUrgent = xmobarColor "#a3be8c" ""
                     , ppLayout = xmobarColor "#4f5b66" ""
                     , ppTitle =  xmobarColor "#c0c5ce" "" . shorten 80
                     , ppSep = xmobarColor "#4f5b66" "" "  "
                     }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Keyboard shortcuts
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching apps
    [ ((modMask,                 xK_Return), safeSpawn "urxvt" [])
    , ((modMask,                 xK_p     ), safeSpawn "rofi" ["-show", "run"])
    , ((modMask,                 xK_Tab   ), safeSpawn "rofi" ["-show", "window"])
    , ((modMask .|. controlMask, xK_f     ), safeSpawn "firefox" [])
    , ((modMask,                 xK_n     ), safeSpawn "thunar" [])
    -- launching cli apps
    , ((modMask .|. controlMask, xK_b     ), safeSpawn "urxvtc" ["-name", "ncmpcpp", "-e", "ncmpcpp"])
    , ((modMask .|. controlMask, xK_n     ), safeSpawn "urxvtc" ["-name", "ranger", "-e", "ranger"])
    , ((modMask .|. controlMask, xK_v     ), safeSpawn "urxvtc" ["-name", "vim", "-e", "nvim"])
    , ((modMask .|. controlMask, xK_m     ), safeSpawn "urxvtc" ["-name", "mutt", "-e", "mutt"])
    -- Kill windows
    , ((modMask .|. shiftMask, xK_c     ), kill)
    -- lock screen
    , ((modMask .|. controlMask, xK_Delete), safeSpawn "rmlock.sh" [])
    -- screenshot
    , ((0, xK_Print                       ), safeSpawn "scrot" [])
    -- multimedia
    , ((modMask,                 xK_u   ), safeSpawn "xfce4-screenshooter" [])
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "pamixer" ["-i", "5"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "pamixer" ["-d", "5"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "pamixer" ["-t"])
    , ((0, 0x1008ff03                   ), safeSpawn "xbacklight" ["-dec", "4"])
    , ((0, 0x1008ff02                   ), safeSpawn "xbacklight" ["-inc", "4"])
    , ((0, xF86XK_AudioPlay             ), safeSpawn "mpc" ["toggle"])
  --, ((modMask,                 xK_Up),   safeSpawn "mpc" ["stop"])
    , ((0, xF86XK_AudioNext             ), safeSpawn "mpc" ["prev"])
    , ((0, xF86XK_AudioPrev             ), safeSpawn "mpc" ["next"])
    , ((0, xF86XK_Display               ), safeSpawn "lxrandr" [])
    , ((0, xF86XK_ScreenSaver           ), safeSpawn "slock" [])
    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    --, ((modMask,               xK_n     ), refresh)

    -- focus
    --, ((modMask,               xK_Tab   ), windows W.focusDown)
    --, ((modMask,               xK_j     ), windows W.focusDown)
    --, ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.shiftMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    --, ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    --, ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_Escape  ), io (exitWith ExitSuccess))
    , ((modMask              , xK_Escape  ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (workspaces conf)[ xK_1
                                         , xK_2
                                         , xK_3
                                         , xK_4
                                         , xK_q
                                         , xK_w
                                         , xK_e
                                         , xK_r
                                         ] ,
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
