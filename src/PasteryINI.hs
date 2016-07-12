{-# LANGUAGE OverloadedStrings #-}

module PasteryINI
    ( get_api_key
    ) where

import qualified Data.HashMap.Strict as H
import Data.Ini
import Data.Text (Text)
import Data.Text.Format

import System.Directory
import System.FilePath

get_api_key :: IO Text
get_api_key = do
    cfg_path <- get_cfg_path
    ini_data <- readIniFile cfg_path
    let ini_map = either (cfg_ferr cfg_path) unIni ini_data
    let sec = case H.lookup "pastery" ini_map of
                Just h -> h
                Nothing -> pastery_sec_err cfg_path
    let api_key = case H.lookup "api_key" sec of
                    Just s -> s
                    Nothing -> key_err "api_key" "pastery" cfg_path
    return api_key

get_cfg_path = do
    home <- getHomeDirectory
    return $ full_path home ".config/bakeit.cfg"

full_path :: String -> String -> String
full_path pref suff = joinPath [pref, suff]

pastery_sec_err :: String -> t
pastery_sec_err cfg_path = cfg_sec_err "pastery" cfg_path

cfg_sec_err :: String -> String -> t
cfg_sec_err sec fname =
    error msg
      where msg = "[" ++ sec ++ "] section not found.\n \
                  \Please add a [" ++ sec ++ "] section to the "
                  ++ fname ++ " file and try again."

cfg_ferr :: String -> String -> t
cfg_ferr cfg_path err =
    error msg
      where msg = "Config file error: "
                  ++ err ++ ".\nMake sure you have a config file at "
                  ++ cfg_path
                  ++ "\nwith a [pastery] section containing your Pastery API key,\n \
                    \which you can get from your https://www.pastery.net account page."

key_err :: String -> String -> String -> t
key_err k sec fname =
    error msg
      where msg = "No " ++ k ++ " entry found.\nPlease add an entry for " ++ k
                  ++ " to the [" ++ sec ++ "] section\nof file "
                  ++ fname ++ " with your API key in it.\nYou can find the \
                     \latter on your account page on https://www.pastery.net."
