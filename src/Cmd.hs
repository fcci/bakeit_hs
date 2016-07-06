module Cmd
    (
      BakeitArgs(..)
    , bakeit_args
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative

data BakeitArgs = BakeitArgs
  { title        :: String
  , lang         :: String
  , duration     :: Int
  , max_views    :: Int
  , open_browser :: Bool
  , debug        :: Bool
  , version      :: Bool
  , filename     :: String
  }

bakeit_args :: Parser BakeitArgs
bakeit_args = BakeitArgs
    <$> strOption
        ( long "title"
       <> short 't'
       <> value ""
       <> metavar "TITLE"
       <> help "The title of the paste" )
    <*> strOption
        ( long "lang"
       <> short 'l'
       <> value ""
       <> metavar "LANG"
       <> help "The language highlighter to use" )
    <*> option auto
        ( long "duration"
       <> short 'd'
       <> value 60
       <> metavar "DURATION"
       <> help "The duration the paste should live for" )
    <*> option auto
        ( long "max-views"
       <> short 'v'
       <> value 0
       <> metavar "VIEWS"
       <> help "How many times the paste can be viewed before it expires" )
    <*> switch
        ( long "open-browser"
       <> short 'b'
       <> help "Automatically open a browser window when done" )
    <*> switch
        ( long "debug"
       <> short 'D'
       <> help "Show debug info" )
    <*> switch
        ( long "version"
       <> short 'V'
       <> help "Show version" )
    <*> argument str
        ( metavar "FILE" )

