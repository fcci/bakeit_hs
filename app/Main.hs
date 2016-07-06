module Main where

import Options.Applicative
import Cmd (bakeit_args)
import Lib (upload)

main :: IO ()
main = execParser opts >>= upload
  where
    opts = info (helper <*> bakeit_args)
      ( fullDesc
     <> progDesc "Upload a file or STDIN to Pastery"
     <> header "bakeit - upload file to Pastery" )

