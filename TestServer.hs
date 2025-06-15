{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive  (mk)
import           GHC.Generics         (Generic)
import           Network.HTTP.Types   (hUserAgent)
import           Network.Wai
import Network.Wai.Handler.Warp (run)
import           Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text
import Data.Maybe (fromMaybe)

newtype UserAgent = UserAgent String deriving (Show, Generic)

instance ToJSON UserAgent

type API = "user-agent" :> Header "User-Agent" Text :> Get '[JSON] UserAgent

printHeader :: Maybe Text -> String
printHeader Nothing = "not speciifed"
printHeader (Just s) = unpack s

server :: Maybe Text -> Handler UserAgent
server h = return $ UserAgent (printHeader h)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app

