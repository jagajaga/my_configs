-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs
-- Author: Øyvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
-------------------------------------------------------------------------------
-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86

-- actions
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Tabbed

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
	where
		uhook = withUrgencyHookC NoUrgencyHook urgentConfig
		cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
		pp = customPP
		kb = toggleStrutsKey
		conf = uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
						 , handleEventHook    = fullscreenEventHook
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         , className =? "MPlayer"   --> doFloat
                         , className =? "mplayer2"  --> doFloat
                         , className =? "Gimp"      --> doFloat
                         , className =? "Skype"     --> doShift "IM"
                         , className =? "Vlc"       --> doFloat
						 , insertPosition Below Newer
						 , transience'
                         ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#C98F0A" ""
                     , ppHiddenNoWindows = xmobarColor "#003347" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#003347" ""
                     , ppTitle =  xmobarColor "#429942" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }


myGSNavigation:: TwoD a (Maybe a)
myGSNavigation= makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
           ((0,xK_Escape), cancel)
          ,((0,xK_space), select)
          ,((0,xK_slash) , substringSearch myGSNavigation)
          ,((0,xK_Left)  , move (-1,0)  >> myGSNavigation)
          ,((0,xK_h)     , move (-1,0)  >> myGSNavigation)
          ,((0,xK_Right) , move (1,0)   >> myGSNavigation)
          ,((0,xK_l)     , move (1,0)   >> myGSNavigation)
          ,((0,xK_Down)  , move (0,1)   >> myGSNavigation)
          ,((0,xK_j)     , move (0,1)   >> myGSNavigation)
          ,((0,xK_Up)    , move (0,-1)  >> myGSNavigation)
          ,((0,xK_k)     , move (0,-1)   >> myGSNavigation)
          ,((0,xK_y)     , move (-1,-1) >> myGSNavigation)
          ,((0,xK_i)     , move (1,-1)  >> myGSNavigation)
          ,((0,xK_n)     , move (-1,1)  >> myGSNavigation)
          ,((0,xK_m)     , move (1,-1)  >> myGSNavigation)
          ]
        -- The navigation handler ignores unknown key symbols
        navDefaultHandler = const myGSNavigation

-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160
							, gs_navigate = myGSNavigation
}

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1
normalBorderColor'  = "#333333"
focusedBorderColor' = "#AFAF87"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["All", "Programming", "Work", "IM", "Media", "Etc", "7", "8", "9"]

-- layouts
layoutHook' = (tile ||| mtile ||| tab ||| full)
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = renamed [Replace "[]="] $ maximize $ minimize $ smartBorders rt
    mtile = renamed [Replace "M[]="] $ maximize $ minimize $ smartBorders $ Mirror rt
    tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full = renamed [Replace "[]"] $ noBorders Full

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "xfce4-terminal"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_e     ), safeSpawn (XMonad.terminal conf) [])
    , ((modMask,               xK_r     ), safeSpawn "dmenu_run" [])
    , ((modMask,               xK_w     ), safeSpawn "chromium" [])
    , ((modMask .|. shiftMask, xK_p     ), safeSpawn "gmrun" [])
    , ((modMask, xK_c     ), kill)

    -- multimedia
-- Alsa mixer bindings
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
--}
{--
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "ponymix" ["increase", "5"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "ponymix" ["decrease", "5"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "ponymix" ["toggle"])
	--}
    , ((modMask, xK_F3             ), safeSpawn "mocp" ["-G"])
    , ((modMask, xK_F4             ), safeSpawn "mocp" ["-f"])
    , ((0, xF86XK_Launch6             ), safeSpawn "autocpu" [])
    , ((modMask, xF86XK_Launch6             ), safeSpawn "autocpu" ["-n"])

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)


    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)
	, ((modMask,               xK_n     ), withFocused minimizeWindow)
    , ((modMask .|. shiftMask, xK_n     ), sendMessage RestoreNextMinimizedWin)
	, ((modMask, xK_m     ), withFocused $ sendMessage . maximizeRestore)
 

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_d, xK_f] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
