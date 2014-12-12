import qualified Data.Map                     as M
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types
import           Prelude
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run

import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

main :: IO ()
main = do
    spawnPipe "xrandr --output VGA-0 --auto --left-of DVI-0"
    xmonad =<< statusBar cmd pp kb conf
    where
        uhook = withUrgencyHookC NoUrgencyHook urgentConfig
        cmd   = "taffybar"
        pp    = defaultPP
        kb    = toggleStrutsKey
        conf  = ewmh $ uhook $ myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces  = workspaces'
                         , modMask            = modMask'
                         , borderWidth        = borderWidth'
                         , normalBorderColor  = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal           = terminal'
                         , keys               = keys'
                         , layoutHook         = layoutHook'
                         , manageHook         = manageHook'
                         , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHook <+> dynStatusBarEventHook myStatusBar myStatusBarCleanup
                         , logHook            = ewmhDesktopsLogHook
                         , startupHook        = startup <+> ewmhDesktopsStartup <+> dynStatusBarStartup myStatusBar myStatusBarCleanup
                         }

myStatusBar (S 0) = spawnPipe "taffybar -x 0"
-- myStatusBar (S s) = spawnPipe $ "taffybar -x " ++ show s

myStatusBarCleanup :: IO ()
myStatusBarCleanup = return ()

startup :: X ()
startup = do
    setWMName "LG3D"
    safeSpawn "amixer" ["-q", "set", "Master", "on"]
    spawn "killall -9 taffybar-linux-x86_64"
    spawn "xmodmap -e \"keysym Menu = Super_L\""
    spawn "xfce4-terminal -e \"setxkbmap -layout us,ru(winkeys) -option grp:caps_toggle && exit\""
    spawnOn "IM" "killall gajim; skype"
    spawnOn "IM" "killall gajim; gajim"
    spawnOn "IRC" ("xfce4-terminal --title=weechat -e weechat")
    {-spawn "killall cmatrix || xfce4-terminal --title=cmatrix -e \"cmatrix -bxu 5\" --maximize --geometry=200x100+0+17"-}


-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen                   --> doFullFloat
                         , className =? "Gimp"            --> doFloat
                         , className =? "Skype"           --> doShift "IM"
                         , className =? "Gajim"           --> doShift "IM"
                         , className =? "Steam"           --> doShift "Steam"
                         , className =? "Vlc"             --> doCenterFloat
                         , className =? "Xfce4-notifyd"   --> doF W.focusDown
                         {-, title =? "cmatrix"             --> [>doIgnore <+><] (doRectFloat $ W.RationalRect 0 (17/900) 1 1) <+> doF W.focusDown <+> doF copyToAll-}
                         {-, title =? "cmatrix"             --> placeHook placeOnBottom-}
                         , title =? "cmatrix"             --> doIgnore
                         , title =? "weechat"             --> doShift "IRC"
                         , transience'
                         , isDialog                         --> doCenterFloat
                         , role      =? "pop-up"            --> doCenterFloat
                         ]
                             where role = stringProperty "WM_WINDOW_ROLE"

myGSNavigation :: TwoD a (Maybe a)
myGSNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
           ((0,xK_Escape), cancel)
          ,((0,xK_Return) , select)
          ,((0,xK_slash) , substringSearch myGSNavigation)
          ,((0,xK_Left)  , move (-1,0)  >> myGSNavigation)
          ,((0,xK_h)     , move (-1,0)  >> myGSNavigation)
          ,((0,xK_Right) , move (1,0)   >> myGSNavigation)
          ,((0,xK_l)     , move (1,0)   >> myGSNavigation)
          ,((0,xK_Down)  , move (0,1)   >> myGSNavigation)
          ,((0,xK_j)     , move (0,1)   >> myGSNavigation)
          ,((0,xK_Up)    , move (0,-1)  >> myGSNavigation)
          ,((0,xK_k)     , move (0,-1)  >> myGSNavigation)
          ,((0,xK_y)     , move (-1,-1) >> myGSNavigation)
          ,((0,xK_i)     , move (1,-1)  >> myGSNavigation)
          ,((0,xK_n)     , move (-1,1)  >> myGSNavigation)
          ,((0,xK_m)     , move (1,-1)  >> myGSNavigation)
          ]
        -- The navigation handler ignores unknown key symbols
        navDefaultHandler = const myGSNavigation

-- GridSelect
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig { gs_cellwidth = 160
                            , gs_navigate = myGSNavigation
}

-- urgent notification
urgentConfig :: UrgencyConfig
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth'        = 1
normalBorderColor'  = "#333333"
focusedBorderColor' = "#00FF00"

-- tabs
tabTheme1 :: Theme
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "lightgreen"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = wspaces ++ (map show $ drop (length wspaces) [1..9])
    where
        wspaces = ["General", "Programming", "Work", "IM", "IRC", "Media", "Steam", "Game"]

myLayoutPrompt = inputPromptWithCompl defaultXPConfig "name of processes" (mkComplFunFromList' ["emacs", "dwb"]) ?+ (\r -> spawn $ "pkill -x " ++ r)

-- layouts
layoutHook' = onWorkspace "IM" skypeLayout (tile ||| mtile ||| tab ||| full)
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    skypeLayout = renamed [Replace "[][]"] $ withIM 0.18 (ClassName "Skype" `And` (Not (Role "ConversationsWindow"))) $ reflectHoriz $ withIM 0.18 (ClassName "Gajim") $ reflectHoriz $ Grid
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

interactiveRunInTerm c config = do
    cmds <- io getCommands
    mkXPrompt Shell config (getShellCompl cmds) run
    where run a = unsafeSpawn $ c ++ " " ++ a

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_e     ), safeSpawn (XMonad.terminal conf) [])
    , ((modMask                                                  , xK_r     ), shellPrompt defaultXPConfig)
    , ((modMask .|. shiftMask                                                  , xK_r     ), interactiveRunInTerm (XMonad.terminal conf ++ " -e") defaultXPConfig )
    , ((modMask                                                  , xK_w     ), safeSpawn "dwb" [])
    , ((modMask .|. shiftMask    , xK_w     ), safeSpawn "chromium" [])
    , ((modMask                                                  , xK_c     ), kill)
    , ((modMask .|. controlMask, xK_space       ),  windowPromptGoto defaultXPConfig )
    , ((modMask                                                  , xK_a     ), safeSpawn "xfe" [])

    -- multimedia
-- Alsa mixer bindings
    , ((0                                                        , xF86XK_AudioRaiseVolume ) , spawn "amixer -q set Master 3+ && /home/jaga/myscripts/getvolume.sh -s")
    , ((0                                                        , xF86XK_AudioLowerVolume ) , spawn "amixer -q set Master 3- && /home/jaga/myscripts/getvolume.sh -s")
    , ((0                                                        , xF86XK_AudioMute        ) , safeSpawn "amixer" ["-q", "set", "Master", "0"])
    , ((modMask                                                  , xK_F12                  ) , spawn "amixer -q set Master 9+ && bash /home/jaga/myscripts/getvolume.sh -s")
    , ((modMask                                                  , xK_p                  ) , spawn "xrandr --output DVI-0 --off --output VGA-0 --auto && slimlock && xrandr --output VGA-0 --auto --left-of DVI-0 --output DVI-0 --auto")
    , ((modMask                                                  , xK_F11                  ) , spawn "amixer -q set Master 9- && bash \"/home/jaga/myscripts/getvolume.sh -s\"")
    , ((modMask                                                  , xK_F9                   ) , spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")

    , ((modMask                                                  , xK_F10                  ) , safeSpawn "amixer" ["-q", "set", "Master", "0"])
--}
    , ((0                                                        , xF86XK_AudioPlay        ) , safeSpawn "mocp" ["-G"])
    , ((0                                                        , xF86XK_AudioNext        ) , safeSpawn "mocp" ["-f"])
    , ((0                                                        , xF86XK_AudioPrev        ) , safeSpawn "mocp" ["-r"])
    , ((0                                                        , xF86XK_AudioStop        ) , safeSpawn "mocp" ["-s"])
    , ((0                                                        , xF86XK_AudioMedia       ) , spawn "xfce4-terminal -e mocp")
    , ((0                                                        , xF86XK_Sleep            ) , spawn "bash /home/jaga/myscripts/lockandsuspend.sh")
    , ((0                                                        , xK_Pause                ) , safeSpawn "bash /home/jaga/myscripts/autocpu.sh" [])
    , ((0                                                        , xK_Print                ) , spawn "import -window root /home/jaga/Dropbox/screenshots/`date +%F_%T`.png" )
    , ((modMask                                                  , xF86XK_Launch6          ) , safeSpawn "autocpu" ["-n"])
    , ((modMask                                                  , xK_t                    ) , spawn "bash /home/jaga/myscripts/screen-translate.sh")
    , ((modMask .|. shiftMask                                    , xK_t                    ) , spawn "bash /home/jaga/myscripts/screen-translate.sh en")

    -- grid
    , ((modMask                                                  , xK_s     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask                                                  , xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask                                    , xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask .|. shiftMask                                    , xK_f     ), withFocused $ windows . W.sink)
    , ((modMask                                                  , xK_f     ), withFocused $ windows . (flip W.float) (W.RationalRect (0) (1/50) (1/1) (1/1))) --TODO
    , ((modMask                                                  , xK_z     ), toggleWS)


    -- focus
    , ((modMask                                                  , xK_Tab   ), windows W.focusDown)
    , ((modMask                                                  , xK_j     ), windows W.focusDown)
    , ((modMask                                                  , xK_k     ), windows W.focusUp)
    , ((modMask                                                  , xK_y     ), windows W.focusMaster)
    , ((modMask                                                  , xK_n     ), withFocused minimizeWindow)
    , ((modMask .|. shiftMask                                    , xK_n     ), sendMessage RestoreNextMinimizedWin)
    , ((modMask                                                  , xK_m     ), withFocused $ sendMessage . maximizeRestore)


    -- swapping
    , ((modMask .|. shiftMask                                    , xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask                                    , xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask                                    , xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask                                                  , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask                                                  , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask                                                  , xK_h     ), sendMessage Shrink)
    , ((modMask                                                  , xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask                                    , xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask                                    , xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask                                    , xK_q     ), io (exitWith ExitSuccess))
    , ((modMask                                                  , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    []

-------------------------------------------------------------------------------

