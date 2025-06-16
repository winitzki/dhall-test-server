{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive  (mk)
import           Network.HTTP.Types   (hUserAgent)
import           Network.Wai
import Network.Wai.Handler.Warp (run)
import           Servant
import Data.Aeson  (ToJSON, Options(..), defaultOptions, genericToJSON, object, Object, (.=), Value)
import Data.Aeson.Types
import GHC.Generics
import Data.Text (Text, unpack)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Data.List
import Servant
import Servant.Server
import Data.Char (chr, ord, isAlphaNum)
import qualified Data.CaseInsensitive as CI

newtype UserAgent = UserAgent
  { userAgent :: String
  } deriving (Eq, Show, Generic)

newtype AllHeaders = AllHeaders
  { headers :: Object -- Aeson.Object is a Map Text Value
  } deriving (Show, Generic)


instance ToJSON AllHeaders where
      toJSON (AllHeaders hdrs) = object ["headers" .= hdrs]


-- Custom Aeson Options to make Kebab Case fields.
userAgentOptions :: Options
userAgentOptions = defaultOptions { fieldLabelModifier = toKebabCase }
  where
    toKebabCase "" = ""
    toKebabCase (c:cs) = toLower c : Data.List.concatMap (\x -> if x `Data.List.elem` ['A'..'Z'] then ['-', toLower x] else [x]) cs


instance ToJSON UserAgent where
  toJSON = genericToJSON userAgentOptions


type API = "user-agent" :> Header "User-Agent" Text :> Get '[JSON] UserAgent
      :<|> "random-string"                          :> Get '[PlainText] String
      :<|> "headers" :> ReqHeaders :> Get '[JSON] AllHeaders -- Raw  :<|> handleAllHeaders

-- TODO: Add other APIs that are required by dhall tests.

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

server :: (Maybe Text -> Handler UserAgent) :<|> Handler String
server = (return . UserAgent . printHeader)
             :<|> liftIO randomString

api :: Proxy API
api = Proxy

app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = run 8080 app

