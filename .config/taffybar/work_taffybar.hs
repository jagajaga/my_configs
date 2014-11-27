import           System.Taffybar

{-import           System.Taffybar.FreedesktopNotifications-}
import           System.Taffybar.NetMonitor
import           System.Taffybar.Pager
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager
import           System.Taffybar.Weather
import           System.Taffybar.CommandRunner

import           System.Taffybar.Widgets.PollingBar
import           System.Taffybar.Widgets.PollingGraph

import           System.Information.CPU2
import           System.Information.DiskIO
import           System.Information.Memory

import           Graphics.UI.Gtk

import           Data.List

memCallback :: IO Double
memCallback = do
  mi <- parseMeminfo
  return $ memoryUsedRatio mi

tempCallback :: Fractional b => [String] -> IO b
tempCallback cpu = do
    [a] <- getCPUTemp cpu
    return $ (fromIntegral a) / 100.0

mySep :: PagerConfig -> IO Label
mySep cfg = do
  sep <- labelNew (Nothing :: Maybe String)
  labelSetMarkup sep (widgetSep cfg)
  return $ sep

diskIOCallback :: String -> String -> IO [Double]
diskIOCallback disk1 disk2 = do
    a <- getDiskTransfer disk1
    b <- getDiskTransfer disk2
    return $ a ++ b

colorOrange :: String
colorOrange = "#C98F0A"

colorLightGreen :: String
colorLightGreen = "#429942"

myTaffy :: PagerConfig
myTaffy = defaultPagerConfig {
         activeWorkspace = colorize colorLightGreen "" .wrap "⎨" "⎬" . escape
       , hiddenWorkspace = colorize colorOrange "" . escape
       , emptyWorkspace  = colorize "#003347" "" . escape
       , urgentWorkspace = colorize "#FFFF00" "" . wrap "[" "]" . escape
       , activeLayout    = colorize "#003347" "" . escape
       , widgetSep       = colorize colorLightGreen "" " | "
}

colorFunc :: (Num t, Num t1) => t -> (t, t, t1)
colorFunc pct = (pct, 1 - pct, 0)

colorFuncTemp :: (Fractional a, Num t, Ord a) => a -> (a, a, t)
colorFuncTemp pct
    | pct <= 0.55 = (0, 1 - pct, 0)
    | pct < 0.70 = (pct, 1 - pct, 0)
    | pct < 1 = (pct, 0, 0)
    | otherwise = (1, 1, 1)

separator :: IO Widget
separator = do
  sp2 <- mySep myTaffy
  box <- hBoxNew False 0
  boxPackStart box sp2 PackNatural 0
  widgetShowAll box
  return (toWidget box)

simpleText :: String -> String -> IO Widget
simpleText text' color = do
  let text = colorize color "" text'
  sep <- labelNew (Nothing :: Maybe String) 
  labelSetMarkup sep text
  box <- hBoxNew False 0
  boxPackStart box sep PackNatural 0
  widgetShowAll box
  return (toWidget box)

{-
    TODO (to implement)
    cpu freq, moc song, keyboard layout, swap, uptime, essid, locks... 
    https://github.com/jaor/xmobar/tree/master/src/Plugins
-}

main :: IO ()
main = do
  let pager  = taffyPagerNew myTaffy
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.9)
                                                      , (0, 0, 1, 0.9)
                                                      ]
                                  , graphWidth = 35
                                  , graphLabel = Nothing
                                  }
      ioCfg  = defaultGraphConfig { graphDataColors = [ (0.3, 1, 0.3, 1)
                                                      , (0.6, 1, 0.6, 1)
                                                      , (0.3, 0.3, 1, 0.7)
                                                      , (0.6, 0.6, 1, 0.7)
                                                    ]
                                  , graphWidth = 35
                                  , graphLabel = Nothing
                                  }
      clock      = textClockNew Nothing "<span fgcolor='#C98F0A'>%a %b %d </span><span fgcolor='#429942'>%H:%M:%S</span>" 1
      netMonitor = netMonitorNewWith 2.0 "wlp3s0" 0 $ colorize colorOrange "" "WiFi: ⇣" ++ colorize colorLightGreen "" "$inKB$" ++ colorize colorOrange "" " : ⇡" ++ colorize colorLightGreen "" "$outKB$" 
      wea        = weatherNew (defaultWeatherConfig "ULLI") { weatherTemplate = "<span fgcolor='#429942'>$tempC$°C</span>" } 100
      iohdd      = pollingGraphNew ioCfg 1 (diskIOCallback "sdd" "md126")
      mem        = pollingBarNew (defaultBarConfig colorFunc) {barWidth = 10} 3 memCallback
      ---TODO notification note = notifyAreaNew defaultNotificationConfig
      cpuText    = simpleText "Cpu"colorOrange 
      diskText   = simpleText "IO »"colorOrange 
      ramText    = simpleText "Ram"colorOrange 
      cpu        = pollingGraphNew cpuCfg 0.5 $ getCPULoad "cpu"
      temp       = pollingBarNew (defaultBarConfig colorFuncTemp) {barWidth = 10} 2 $ tempCallback ["cpu0"]
      tray       = systrayNew
      mocp       = commandRunnerNew 1 "/home/jaga/myscripts/getmocpinfo.sh" [] "Moc: OFF" "#FFFFFF" 
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = intercalate [separator] [ [clock], [tray], [wea], [mem, ramText], [temp, cpu, cpuText], [iohdd, diskText], [netMonitor], [mocp] ]
                                        , widgetSpacing = 5
                                        , barHeight = 23
                                        }
