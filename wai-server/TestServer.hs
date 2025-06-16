{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status200, status404, status405, hContentType, methodGet)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode, object, (.=), ToJSON(toJSON))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.CaseInsensitive (original, mk)

-- | The main WAI application.
-- It takes a Request and returns a Response.
app :: Request -> (Response -> IO b) -> IO b
app req respond = do
    let path = pathInfo req
    let method = requestMethod req -- Get the HTTP method of the request

    if path == ["headers"] -- Changed from "get_headers"
        then do
            -- Endpoint: /headers
            -- Extracts all request headers and returns them as JSON.
            -- The response will be in the format { "headers" : { "Host": "value", ... } }
            if method == methodGet -- Check if the method is GET
                then do
                    let reqHeaders = requestHeaders req

                    -- Convert the list of headers into a HashMap for JSON serialization.
                    let headersMap = HM.fromList $ map (\(key, value) ->
                                                        (TE.decodeUtf8 (original key), TE.decodeUtf8 value))
                                                  reqHeaders

                    -- Wrap the headers map inside an object with a "headers" key
                    let jsonResponse = encode $ object ["headers" .= headersMap]
                    respond $ responseLBS status200 [(hContentType, "application/json")] jsonResponse
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /headers."

    else if path == ["user-agent"] -- Changed from "get-user-agent"
        then do
            -- Endpoint: /user-agent
            -- Extracts only the User-Agent header and returns it as JSON.
            -- The response will be in the format { "user-agent" : "value" }
            if method == methodGet -- Check if the method is GET
                then do
                    let reqHeaders = requestHeaders req
                    -- Look up the "User-Agent" header. mk converts String to CI ByteString.
                    let userAgentHeader = lookup (mk "User-Agent") reqHeaders

                    -- Create a JSON object for the User-Agent.
                    -- If User-Agent is found, decode it to Text; otherwise, use an empty string.
                    let userAgentValue = case userAgentHeader of
                                           Just val -> TE.decodeUtf8 val
                                           Nothing  -> ""

                    -- Use "user-agent" (lowercase) as the key
                    let jsonResponse = encode $ object ["user-agent" .= userAgentValue]
                    respond $ responseLBS status200 [(hContentType, "application/json")] jsonResponse
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /user-agent."

    else do
        -- For any other path, respond with a 404 Not Found.
        respond $ responseLBS status404 [(hContentType, "text/plain")]
                  "404 Not Found: Supported endpoints are /headers and /user-agent."

-- | Main function to run the Warp server.
-- It will listen on port 8080.
main :: IO ()
main = do
    putStrLn "Listening on http://localhost:8080/"
    putStrLn "Access all headers (GET only) at: http://localhost:8080/headers"
    putStrLn "Access User-Agent header (GET only) at: http://localhost:8080/user-agent"
    run 8080 app
