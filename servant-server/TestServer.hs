{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive  (mk)
import           Network.HTTP.Types   (hUserAgent)
import           Network.Wai
import Network.Wai.Handler.Warp (run)
import           Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Data.List
import Data.Char (chr, ord, isAlphaNum)


type API = "user-agent" :> Header "User-Agent" Text :> Get '[PlainText] String
      :<|> "random-string"                          :> Get '[PlainText] String

-- TODO: add other APIs are required by dhall tests

availableRandomChars :: [Char]
availableRandomChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

alphabetLen = Prelude.length availableRandomChars

getRandomAlphanumericChar :: IO Char
getRandomAlphanumericChar = do
        randomInt <- randomRIO (0, alphabetLen)
        return $ availableRandomChars !! (randomInt `mod` alphabetLen)

randomString :: IO [Char] -- Not `IO String`! That crashes and hangs the server.
randomString = mapM (\_ -> getRandomAlphanumericChar) [1..32]

printHeader :: Maybe Text -> String
printHeader Nothing = "not specified"
printHeader (Just s) = unpack s

server :: (Maybe Text -> Handler String) :<|> Handler String
server = (return . printHeader)
             :<|> liftIO randomString

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app

