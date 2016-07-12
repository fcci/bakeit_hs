{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( upload
    ) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Process
import GHC.IO.Exception
import Cmd
import WebBrowser
import qualified PasteryJSON as PJ
import PasteryINI (get_api_key)

upload :: BakeitArgs -> IO ()
upload (BakeitArgs {version = True}) = putStrLn "Version 0.1.0"
upload args = do
    api_key <- get_api_key
    let qps = qparams args (T.unpack api_key)
    raw_data <- L.readFile $ filename args
    request' <- parseRequest "POST https://www.pastery.net/api/paste/"
    let request
            = setRequestQueryString qps
            $ setRequestBodyLBS raw_data
            $ setRequestHeaders req_hdrs
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpLBS request
    let upload_url = parse_upload_rsp response
    res <- maybe_launch_browser args upload_url
    putStrLn $ "Paste URL: " ++ upload_url

parse_upload_rsp :: Response L.ByteString -> String
parse_upload_rsp response =
    get_upload_url status body
      where
          status = getResponseStatus response
          body = LS8.unpack $ getResponseBody response

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

qparams :: BakeitArgs -> String -> [(S8.ByteString, Maybe S8.ByteString)]
qparams args api_key =
    [make_qp k v | (k, v) <- fields]
      where fields = [("api_key",   api_key),
                      ("title",     title args),
                      ("language",  lang args),
                      ("duration",  show $ duration args),
                      ("max_views", show $ max_views args)
                      ]
            make_qp k v = (S8.pack k, Just $ S8.pack $ v)


get_upload_url :: Status -> String -> String
get_upload_url (Status{statusCode = sc, statusMessage = sm}) body
    | sc >= 300 && sc < 400 =
        error $ "Unexpected redirect: " ++ (show sc) ++ " " ++ (S8.unpack sm)
    | sc == 413 =
        error $ "The chosen file was rejected by the server " ++
                "because it was too large, please try a smaller " ++
                "file."
    | sc == 422 =
        error $ case PJ.parse_error body of
                     Right json -> T.unpack $ PJ.error_msg json
                     Left err -> err
    | sc >= 400 && sc < 500 =
        error $ "There was a problem with the request: " ++
                (show sc) ++ " " ++ (S8.unpack sm)
    | sc >= 500 =
        error $ "There was a server error " ++ (show sc) ++
                "; please try again later."
    | otherwise =
        case PJ.parse_return body of
             Right json -> T.unpack $ PJ.url json
             Left  err  -> error err

