module Lib
    ( upload
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Yaml as Yaml
import Network.HTTP.Simple
import Cmd
import WebBrowser
import System.Process
import GHC.IO.Exception

upload :: BakeitArgs -> IO ()
upload (BakeitArgs {version = True}) = putStrLn "Version 0.1.0"
upload args = do
    raw_data <- L.readFile $ filename args
    request' <- parseRequest "POST https://www.pastery.net/api/paste/"
    let qps = qparams args
    let request
            = setRequestQueryString qps
            $ setRequestBodyLBS raw_data
            $ setRequestHeaders req_hdrs
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpLBS request
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    let upload_url = "https://www.haskell.org/hoogle/?hoogle=lookup"
    LS8.putStrLn (getResponseBody response)
    --putStrLn $ fromJSON upload_url
    res <- maybe_launch_browser args upload_url
    putStrLn "Done"

maybe_launch_browser :: BakeitArgs -> String -> IO ()
maybe_launch_browser (BakeitArgs {Cmd.open_browser = True}) url =
    case WebBrowser.open url of
         Left  err -> return ()
         Right ph  -> ph >> return ()
maybe_launch_browser _ _ = putStrLn "No flag to launch browser"

req_hdrs :: [(CI S8.ByteString, S8.ByteString)]
req_hdrs =
    [make_hdr hn hv | (hn, hv) <- hdrs]
      where make_hdr hn hv = (CI.mk $ S8.pack hn, S8.pack hv)
            hdrs = [("Content-Type", "application/octet-stream"),
                    ("User-Agent", "Mozilla/5.0 (Haskell) bakeit library")]

qparams :: BakeitArgs -> [(S8.ByteString, Maybe S8.ByteString)]
qparams args =
    [make_qp k v | (k, v) <- fields]
      where fields = [("title",     title args),
                      ("language",  lang args),
                      ("duration",  show $ duration args),
                      ("max_views", show $ max_views args)
                      ]
            make_qp k v = (S8.pack k, Just $ S8.pack $ v)

