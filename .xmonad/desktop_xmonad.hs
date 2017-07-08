import qualified Data.Map                         as M
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types
import           Prelude
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.NoBorders
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Pass
import qualified XMonad.StackSet                  as W
import           XMonad.Util.Run

import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

main :: IO ()
main = do
    xmonad =<< statusBar cmd pp kb conf
    where
        uhook = withUrgencyHookC NoUrgencyHook urgentConfig
        cmd   = "taffybar"
        pp    = defaultPP
        kb    = toggleStrutsKey
        conf  = ewmh $ uhook myConfig

myConfig = defaultConfig { workspaces         = myWorkspaces
    , modMask            = modMask'
    , borderWidth        = borderWidth'
    , normalBorderColor  = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , terminal           = terminal'
    , keys               = keys'
    , layoutHook         = layoutHook'
    , manageHook         = manageHook'
    , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHook
    , logHook            = ewmhDesktopsLogHook >> updatePointer (0.5, 0.5) (0.25, 0.25)
    , startupHook        = startup <+> ewmhDesktopsStartup
    }

startup :: X ()
startup = do
    setWMName "LG3D"
    safeSpawn "amixer" ["-q", "set", "Master", "on"]
    spawn     "xmodmap -e \"keysym Menu = Super_L\""
    spawn     "xfce4-terminal -e \"setxkbmap -layout us,ru -variant colemak, -option grp:caps_toggle && exit\""
    spawnOn   "ς"     "skype"
    spawnOn   "ς"     "pidgin"
    spawnOn   "ς"     "slack"
    spawnOn   "ζ"  "steam"
    spawnOn   "ψ"    "xfce4-terminal --title=weechat -e weechat"
    spawnOn   "μ"  "xfce4-terminal --title=mocp -e mocp"
    {-spawn "killall cmatrix || xfce4-terminal --title=cmatrix -e \"cmatrix -bxu 5\" --maximize --geometry=200x100+0+17"-}

manageHook' = composeAll [ 
      isFullscreen                   --> doFullFloat
    , className =? "Skype"           --> doShift "ς"
    , className =? "Slack"         --> doShift "ς"
    , className =? "Pidgin"           --> doShift "ς"
    , className =? "Steam"           --> doShift "ζ"
    , className =? "Vlc"             --> doCenterFloat
    , className =? "Xfce4-notifyd"   --> doF W.focusDown
    {-, title =? "cmatrix"             --> [>doIgnore <+><] (doRectFloat $ W.RationalRect 0 (17/900) 1 1) <+> doF W.focusDown <+> doF copyToAll-}
    {-, title =? "cmatrix"             --> placeHook placeOnBottom-}
    , title =? "cmatrix"             --> doIgnore
    , title =? "weechat"             --> doShift "ψ"
    , title =? "twitter"             --> doShift "ψ"
    , title =? "mocp"                --> doShift "μ"
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

workspacesPrompt :: (String -> X ()) -> X ()
workspacesPrompt action = do
    ws <- allWorkspaces
    let compl = mkComplFunFromList ws
    inputPromptWithCompl defaultXPConfig "Workspace name" compl ?+ action

allWorkspaces :: X [WorkspaceId]
allWorkspaces = withWindowSet (return . map W.tag . W.workspaces)

shiftFocused :: WorkspaceId -> X ()
shiftFocused = windows . W.shift

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
myWorkspaces = wspaces ++ (map show $ drop (length wspaces) [1..9])
    where
        wspaces = ["α", "λ", "ξ", "ς", "ψ", "μ", "ζ", "ο"]

myLayoutPrompt = inputPromptWithCompl defaultXPConfig "name of processes" (mkComplFunFromList' ["emacs", "qutebrowser"]) ?+ (\r -> spawn $ "pkill -x " ++ r)

-- layouts
layoutHook' = onWorkspace "ζ" steamLayout
    $ onWorkspace "ς" skypeLayout
    $ tile ||| mtile ||| tab ||| full ||| oneBig
  where
    rt          = ResizableTall 1 (2/100) (1/2) []
    skypeLayout = renamed [Replace "[][]"]
        $ withIM 0.18 (ClassName "Skype" `And` Not (Role "ConversationsWindow"))
            $ reflectHoriz
                $ withIM 0.18 (ClassName "Pidgin" `And` Role "buddy_list")
                    $ reflectHoriz
                        $ smartBorders
                            $ multiCol [2] 4 0.01 0.5
    steamLayout = renamed [Replace "λ"]
        $ withIM 0.18 (ClassName "ζ" `And` Title "Friends")
            $ multiCol [1] 4 0.01 0.25
    tile        = renamed [Replace "[]="] $ maximize $ minimize $ smartBorders rt
    mtile       = renamed [Replace "M[]="] $ maximize $ minimize $ smartBorders $ Mirror rt
    oneBig      = renamed [Replace "M[]="] $ maximize $ minimize $ smartBorders $ OneBig (3/4) (3/4)
    tab         = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full        = renamed [Replace "[]"] $ noBorders Full

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
    [ ((modMask,                 xK_e     ), safeSpawn (XMonad.terminal conf) [])
    , ((modMask,                 xK_r     ), shellPrompt defaultXPConfig)
    , ((modMask,                 xK_w     ), bindOn [("ζ", spawn "steam"), ("", spawn "qutebrowser")])
    , ((modMask .|. shiftMask,   xK_w     ), safeSpawn "firefox" [])
    , ((modMask,                 xK_c     ), kill)
    , ((modMask .|. controlMask, xK_space ), windowPromptGoto defaultXPConfig )
    , ((modMask .|. controlMask, xK_p ), passPrompt defaultXPConfig )
    , ((modMask,                 xK_a     ), safeSpawn "spacefm" [])

    -- multimedia
-- Alsa mixer bindings
    , ((0,                       xF86XK_AudioRaiseVolume ) , spawn "amixer -q set Master 3+ && /home/jaga/myscripts/getvolume.sh -s")
    , ((0,                       xF86XK_AudioLowerVolume ) , spawn "amixer -q set Master 3- && /home/jaga/myscripts/getvolume.sh -s")
    , ((0,                       xF86XK_AudioMute        ) , safeSpawn "amixer" ["-q", "set", "Master", "0"])
    , ((modMask,                 xK_F12                  ) , spawn "amixer -q set Master 9+ && bash /home/jaga/myscripts/getvolume.sh -s")
    , ((modMask,                 xK_F11                  ) , spawn "amixer -q set Master 9- && bash \"/home/jaga/myscripts/getvolume.sh -s\"")
    , ((modMask,                 xK_F9                   ) , spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")

    , ((modMask,                 xK_F10                  ) , safeSpawn "amixer" ["-q", "set", "Master", "0"])
--}
    , ((0,                       xF86XK_AudioPlay        ) , safeSpawn "mocp" ["-G"])
    , ((0,                       xF86XK_AudioNext        ) , safeSpawn "mocp" ["-f"])
    , ((0,                       xF86XK_AudioPrev        ) , safeSpawn "mocp" ["-r"])
    , ((0,                       xF86XK_AudioStop        ) , safeSpawn "mocp" ["-s"])
    , ((0,                       xF86XK_Tools       ) , safeSpawn "lovemoc" [ ])
    , ((0,                       xF86XK_Sleep            ) , spawn "bash /home/jaga/myscripts/lockandsuspend.sh")
    , ((0,                       xK_Pause                ) , safeSpawn "bash /home/jaga/myscripts/autocpu.sh" [])
    , ((0,                       xK_Print                ) , spawn "import -window root /home/jaga/Dropbox/screenshots/`date +%F_%T`.png" )
    , ((modMask,                 xF86XK_Launch6          ) , safeSpawn "autocpu" ["-n"])
    , ((modMask,                 xK_t                    ) , spawn "bash /home/jaga/myscripts/screen-translate.sh")
    , ((modMask .|. shiftMask,   xK_t                    ) , spawn "bash /home/jaga/myscripts/screen-translate.sh en")

    -- grid
    , ((modMask,                 xK_s     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,                 xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask .|. shiftMask,   xK_f     ), withFocused $ windows . W.sink)
    , ((modMask,                 xK_f     ), withFocused $ windows . flip W.float (W.RationalRect 0 (1/50) 1 1))
    , ((modMask,                 xK_z     ), toggleWS)
    , ((modMask .|. shiftMask,   xK_b     ), withFocused toggleBorder)

    , ((modMask,                 xK_backslash), workspacesPrompt addWorkspace)
    , ((modMask .|. controlMask, xK_backslash), removeEmptyWorkspace)

    , ((modMask .|. shiftMask,   xK_backslash), workspacesPrompt $ \wid -> addHiddenWorkspace wid >> shiftFocused wid)


    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_y     ), windows W.focusMaster)
    , ((modMask,               xK_n     ), withFocused minimizeWindow)
    , ((modMask .|. shiftMask, xK_n     ), sendMessage RestoreNextMinimizedWin)
    , ((modMask,               xK_m     ), withFocused $ sendMessage . maximizeRestore)


    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask,               xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    zip (zip (repeat modMask) [xK_1..xK_9]) (map (withNthWorkspace W.view) [0..])
    ++
    zip (zip (repeat (modMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
    ++
    [((m .|. modMask .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    []
