{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types (status200, status403, status404, status405, hContentType, methodGet)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode, object, (.=), ToJSON(toJSON))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.CaseInsensitive (original, mk)
import System.Random (newStdGen, randomRs)
import Data.Char (isAlphaNum)

-- | Statically defined list of alphanumeric characters for random string generation.
-- This avoids re-filtering on every call to generateRandomAlphanumericString.
alphanumericChars :: [Char]
alphanumericChars = filter isAlphaNum ['!'..'~']

-- | Fixed ByteString response for the /xyz endpoint.
-- This is now a top-level static definition.
fixedXYZResponse :: ByteString
fixedXYZResponse = "1234"

-- | Generates a random alphanumeric string of a given length.
generateRandomAlphanumericString :: Int -> IO T.Text
generateRandomAlphanumericString len = do
    g <- newStdGen -- Get a new random number generator
    -- Use the statically defined alphanumericChars
    let randomIndices = take len (randomRs (0, length alphanumericChars - 1) g)
    return $ T.pack $ map (\i -> alphanumericChars !! i) randomIndices

-- | The main WAI application.
-- It takes a Request and returns a Response.
app :: Request -> (Response -> IO b) -> IO b
app req respond = do
    let path = pathInfo req
    let method = requestMethod req -- Get the HTTP method of the request

    if path == ["headers"]
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

    else if path == ["user-agent"]
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

    else if path == ["random-string"]
        then do
            -- Endpoint: /random-string
            -- Returns a random alphanumeric string with 32 characters.
            -- The response will be in plain text.
            if method == methodGet -- Check if the method is GET
                then do
                    randomStr <- generateRandomAlphanumericString 32
                    -- Respond with plain text, encoding the Text to a lazy ByteString
                    respond $ responseLBS status200 [(hContentType, "text/plain")] (LBS.fromStrict $ TE.encodeUtf8 randomStr)
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /random-string."

    else if path == ["foo"]
        then do
            -- Endpoint: /foo
            -- Requires a 'Test' header to be present for a successful response.
            -- Returns 403 Forbidden if 'Test' header is missing.
            if method == methodGet -- Check if the method is GET
                then do
                    let reqHeaders = requestHeaders req
                    -- Check for the presence of the "Test" header.
                    let testHeaderPresent = lookup (mk "Test") reqHeaders /= Nothing

                    if testHeaderPresent
                        then do
                            -- If "Test" header is present, return "./bar"
                            respond $ responseLBS status200 [(hContentType, "text/plain")] "./bar"
                        else do
                            -- If "Test" header is missing, return 403 Forbidden
                            respond $ responseLBS status403 [(hContentType, "text/plain")]
                                      "403 Forbidden: 'Test' header is required."
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /foo."

    else if path == ["bar"]
        then do
            -- Endpoint: /bar
            -- Requires a 'Test' header to be present for a successful response.
            -- Returns 403 Forbidden if 'Test' header is missing.
            -- Returns plain text "True" if 'Test' header is present.
            if method == methodGet -- Check if the method is GET
                then do
                    let reqHeaders = requestHeaders req
                    -- Check for the presence of the "Test" header.
                    let testHeaderPresent = lookup (mk "Test") reqHeaders /= Nothing

                    if testHeaderPresent
                        then do
                            -- If "Test" header is present, return "True"
                            respond $ responseLBS status200 [(hContentType, "text/plain")] "True"
                        else do
                            -- If "Test" header is missing, return 403 Forbidden
                            respond $ responseLBS status403 [(hContentType, "text/plain")]
                                      "403 Forbidden: 'Test' header is required."
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /bar."

    else if path == ["nonexistent-file.dhall"]
        then do
            -- Endpoint: /nonexistent-file.dhall
            -- Always returns 404 Not Found, regardless of method or headers, with a specific message.
            respond $ responseLBS status404 [(hContentType, "text/plain")] "404 Not Found"

    else if path == ["xyz"]
        then do
            -- Endpoint: /xyz
            -- Returns a fixed ByteString value "1234" as plain text.
            if method == methodGet -- Check if the method is GET
                then do
                    -- Use the statically defined fixedXYZResponse
                    respond $ responseLBS status200 [(hContentType, "text/plain")] fixedXYZResponse
                else do
                    -- If method is not GET, return 405 Method Not Allowed
                    respond $ responseLBS status405 [(hContentType, "text/plain")]
                              "405 Method Not Allowed: Only GET method is supported for /xyz."

    else do
        -- For any other path, respond with a general 404 Not Found.
        respond $ responseLBS status404 [(hContentType, "text/plain")]
                  "404 Not Found: Supported endpoints are /headers, /user-agent, /random-string, /foo, /bar, /nonexistent-file.dhall, and /xyz."

-- | Main function to run the Warp server.
-- It will listen on port 8080.
main :: IO ()
main = do
    putStrLn "Listening on http://localhost:8080/"
    putStrLn "Access all headers (GET only) at: http://localhost:8080/headers"
    putStrLn "Access User-Agent header (GET only) at: http://localhost:8080/user-agent"
    putStrLn "Access a random 32-character string (GET only) at: http://localhost:8080/random-string"
    putStrLn "Access /foo (GET only, requires 'Test' header) at: http://localhost:8080/foo"
    putStrLn "Access /bar (GET only, requires 'Test' header) at: http://localhost:8080/bar"
    putStrLn "Access /nonexistent-file.dhall (always 404) at: http://localhost:8080/nonexistent-file.dhall"
    putStrLn "Access /xyz (GET only) at: http://localhost:8080/xyz"
    run 8080 app
