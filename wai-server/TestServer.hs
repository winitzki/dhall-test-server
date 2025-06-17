{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} -- Required for Data.FileEmbed

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
import Data.FileEmbed (embedDir) -- Import for embedding directory contents
import qualified Data.List as L -- For list operations like lookup

-- | Statically defined list of alphanumeric characters for random string generation.
-- This avoids re-filtering on every call to generateRandomAlphanumericString.
alphanumericChars :: [Char]
alphanumericChars = filter isAlphaNum ['!'..'~']

-- | Fixed ByteString response for the /xyz endpoint.
-- This is now a top-level static definition.
fixedXYZResponse :: ByteString
fixedXYZResponse = "1234"

-- | Embedded static files from the 'resources' directory.
-- In a real scenario, this would look for files in ./resources at compile time.
-- For demonstration, we simulate embedding a single file "hello.txt".
-- If you had a real 'resources' directory, you would use:
-- staticFiles :: [(FilePath, BS.ByteString)]
-- staticFiles = $(embedDir "resources")
--
-- We'll manually create a list that mimics the output of embedDir for a simple case.
-- The FilePath from embedDir is a String, so we'll convert it to Text for path comparison.
-- The paths here are relative to the 'resources' directory itself.
staticFileContents :: [(T.Text, BS.ByteString)]
staticFileContents =
    [ ("hello.txt", "Hello from the embedded static file! This is plain text content.")
    ]

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

    -- New logic for serving static files under the /static/ prefix
    else if method == methodGet && (not (null path) && head path == "static")
        then do
            -- Extract the path relative to the 'static' prefix
            let staticFilePathSegments = tail path
            let requestedStaticPath = T.intercalate "/" staticFilePathSegments

            -- Find the file content in our embedded resources
            case L.lookup requestedStaticPath staticFileContents of
                Just fileContent -> do
                    -- If found, respond with the file content as plain text
                    respond $ responseLBS status200 [(hContentType, "text/plain")] (LBS.fromStrict fileContent)
                Nothing -> do
                    -- If not found in static files, return a specific 404 for static files
                    respond $ responseLBS status404 [(hContentType, "text/plain")]
                              "404 Not Found: The requested static file was not found under /static/."
        else do
            -- For any other path or non-GET method not handled by specific endpoints, respond with a general 404 Not Found.
            respond $ responseLBS status404 [(hContentType, "text/plain")]
                      "404 Not Found: Supported endpoints are /headers, /user-agent, /random-string, /foo, /bar, /nonexistent-file.dhall, /xyz, and static files under /static/."

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
    putStrLn "Access static files (GET only) from the /resources directory via /static/ (e.g., http://localhost:8080/static/hello.txt)"
    run 8080 app
