{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode, object, (.=), ToJSON(toJSON))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.CaseInsensitive (original)

-- | The main WAI application.
-- It takes a Request and returns a Response.
app :: Request -> (Response -> IO b) -> IO b
app req respond = do
    -- Extract all headers from the request.
    -- Headers are a list of (CI ByteString, ByteString).
    -- Renamed the local variable to 'reqHeaders' to avoid name collision.
    let reqHeaders = requestHeaders req

    -- Convert the list of headers into a HashMap for JSON serialization.
    -- We convert CaseInsensitive ByteString keys and ByteString values to Text.
    let headersMap = HM.fromList $ map (\(key, value) ->
                                        (TE.decodeUtf8 (original key), TE.decodeUtf8 value))
                                  reqHeaders

    -- Encode the HashMap of headers into a JSON ByteString.
    let jsonResponse = encode $ toJSON headersMap

    -- Respond with a 200 OK status, Content-Type: application/json,
    -- and the JSON ByteString as the body.
    respond $ responseLBS status200 [(hContentType, "application/json")] jsonResponse

-- | Main function to run the Warp server.
-- It will listen on port 3000.
main :: IO ()
main = do
    putStrLn "Listening on http://localhost:3000/"
    run 3000 app
