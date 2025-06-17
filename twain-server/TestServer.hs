{-# language OverloadedStrings #-}

import Network.Wai.Handler.Warp (run)
import Web.Twain

main :: IO ()
main = do
  run 8080 $
    foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ get "/" index
  , post "/echo/:name" echoName
  ]

index :: ResponderM a
index = send $ html "Hello World!"

echoName :: ResponderM a
echoName = do
  name <- param "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."

