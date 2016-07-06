{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module PasteryJSON where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as DBC
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import GHC.Generics

data PasteryReturn = PasteryReturn
  { id            :: !Text
  , duration      :: Int
  , title         :: !Text
  , url           :: !Text
  , language      :: !Text
  } deriving (Generic, Show)

instance FromJSON PasteryReturn
instance ToJSON PasteryReturn

parse_return :: String -> (Either String PasteryReturn)
parse_return s =
    (eitherDecode $ DBC.pack s) :: (Either String PasteryReturn)

data PasteryError = PasteryError
  { result    :: !Text
  , error_msg :: !Text
  } deriving (Generic, Show)

instance FromJSON PasteryError
instance ToJSON PasteryError

parse_error :: String -> (Either String PasteryError)
parse_error s = (eitherDecode $ DBC.pack s) :: (Either String PasteryError)

{-# LANGUAGE DeriveDataTypeable #-}
