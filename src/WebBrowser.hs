module WebBrowser
    ( open
    , open_browser
    , Browser
    , ProcessHandle
    ) where

import System.Info
import System.Process

data Browser =
      Brw_default
    | Brw_firefox
    | Brw_internet_explorer
    | Brw_chrome
    | Brw_opera
    | Brw_safari
    deriving (Show, Eq)

data OSType =
      Os_linux
    | Os_macos
    | Os_win32
    deriving (Show, Eq)

type LaunchFunction = (Either (String -> String) (String -> IO ProcessHandle))
type LaunchResult = (Either String (IO ProcessHandle))
type LaunchCmd = (Browser -> LaunchFunction)

-- In MacOS
-- * To open the default browser: "open $url"
-- * To open a specific browser: "open -a $browser $url"
-- In Linux
-- * To open the default browser in an X environment: "xdg-open $url"
-- In Windows
-- * To open the default browser: "start link $url"
--
open :: String -> LaunchResult
open url = open_browser Brw_default url

open_browser :: Browser -> String -> LaunchResult
open_browser b url =
    case os_type of
         Just ost -> launch_browser ost b url
         Nothing  -> error $ "Unsupported OS: " ++ os

launch_browser :: OSType -> Browser -> String -> LaunchResult
launch_browser ost b url =
    case lookup ost cmds of
         Just cmd -> launch_cmd cmd b url
         Nothing  -> error $ "Internal error: no command for " ++ os_name ost

launch_cmd :: LaunchCmd -> Browser -> String -> LaunchResult
launch_cmd cmd b url =
    case cmd b of
         Right launch -> Right $ launch url
         Left  err    -> Left $ err $ browser_name b

cmds :: [(OSType, LaunchCmd)]
cmds = [ (Os_macos, macos_cmd)
       , (Os_linux, linux_cmd)
       , (Os_win32, win32_cmd)
       ]

macos_cmd :: Browser -> LaunchFunction
macos_cmd Brw_default = Right $ \url -> spawnCommand $ "open " ++ url
macos_cmd _           = Right $ \url -> spawnCommand $ "open -a " ++ url

linux_cmd :: Browser -> LaunchFunction
linux_cmd Brw_default = Right $ \url -> spawnCommand $ "xdg-open " ++ url
linux_cmd _           = Left  $ \browser -> error $ error_default browser

win32_cmd :: Browser -> LaunchFunction
win32_cmd Brw_default = Right $ \url -> spawnCommand $ "start_link " ++ url
win32_cmd _           = Left  $ \browser -> error $ error_default browser

os_type :: Maybe OSType
os_type =
    case os of
        "darwin"  -> Just Os_macos
        "linux"   -> Just Os_linux
        "mingw32" -> Just Os_win32
        _         -> Nothing

os_name :: OSType -> String
os_name os_t =
    case os_t of
        Os_macos -> "MacOS"
        Os_linux -> "Linux"
        Os_win32 -> "Windows"

browser_name :: Browser -> String
browser_name b =
    case lookup b browser_names of
         Just s  -> s
         Nothing -> error $ "Internal error: No browser name for " ++ (show b)

browser_names :: [(Browser, String)]
browser_names = [ (Brw_firefox          , "Firefox")
                , (Brw_internet_explorer, "Internet Explorer")
                , (Brw_chrome           , "Google Chrome")
                , (Brw_opera            , "Opera")
                , (Brw_safari           , "Safari")
                ]

error_default :: String -> String
error_default b =
    error "Browser " ++ b ++ " is unsupported. " ++
          "Only the default browser is supported on this platform right now"

error_unsupported :: OSType -> String
error_unsupported p =
    error "Platform " ++ os_name p ++
          " not yet supported by this library"


