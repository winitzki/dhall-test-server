#!/bin/bash
set -e # Exit immediately if a command exits with a non-zero status.

# --- Configuration ---
HASKELL_FILE="TestServer.hs" # Changed to TestServer.hs
EXECUTABLE_NAME="dhall-test-server" # Changed to dhall-test-server
PORT="8080"
BASE_URL="http://localhost:${PORT}"
PID_FILE="/tmp/haskell_server_${PORT}.pid" # File to store the server's PID

# --- Cleanup Function ---
cleanup() {
    echo "--- Cleaning up... ---"
    if [ -f "${PID_FILE}" ]; then
        PID=$(cat "${PID_FILE}")
        if kill -0 "${PID}" 2>/dev/null; then # Check if process exists
            echo "Killing server process (PID: ${PID})..."
            kill "${PID}"
            wait "${PID}" 2>/dev/null || true # Wait for process to terminate, ignore error if already dead
        else
            echo "No running server found with PID ${PID}."
        fi
        rm -f "${PID_FILE}"
    else
        echo "No PID file found, server might not have started or already cleaned up."
    fi
    # Optional: uncomment to clean cabal build artifacts
    # echo "Running cabal clean..."
    # cabal clean
    echo "Cleanup complete."
}

# --- Register cleanup function to run on exit ---
trap cleanup EXIT

# --- Build the Haskell executable using Cabal ---
echo "--- Building Haskell server with Cabal ---"
# Ensure cabal is up to date and dependencies are installed
cabal update
cabal build ${EXECUTABLE_NAME} --ghc-options=-dynamic --with-compiler=/Users/user/.ghcup/bin/ghc-9.8.4 # Builds the executable target defined in your .cabal file
if [ $? -ne 0 ]; then
    echo "Haskell build failed!"
    exit 1
fi
echo "Haskell server built successfully with Cabal."

# --- Run the Haskell server in the background ---
echo "--- Starting Haskell server on port ${PORT} ---"
# Find the path to the executable built by cabal
# cabal list-bin is the most reliable way to get the path
SERVER_PATH=$(cabal list-bin ${EXECUTABLE_NAME} --ghc-options=-dynamic --with-compiler=/Users/user/.ghcup/bin/ghc-9.8.4) # Added cabal options
# Check if the server path exists
if [ ! -f "${SERVER_PATH}" ]; then
    echo "Error: Executable not found at ${SERVER_PATH}. Ensure it's built and 'cabal list-bin ${EXECUTABLE_NAME}' works."
    exit 1
fi

"${SERVER_PATH}" & echo $! > "${PID_FILE}" # Run in background and store PID
echo "Server started with PID $(cat "${PID_FILE}"). Waiting for it to come online..."
sleep 2 # Give the server a moment to start up

# --- Test Endpoints ---
echo "--- Testing Endpoints ---"
SUCCESS_COUNT=0
TOTAL_TESTS=0

# Helper function to test an endpoint
test_endpoint() {
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    local url=$1
    local expected_status=$2
    local expected_body_regex=$3 # Use regex for flexible matching, or exact string
    local headers=$4 # Optional headers

    echo -n "Testing ${url} (Expected: ${expected_status} ${expected_body_regex:-<any>})... "

    # Capture both status code and response body
    # Removed quotes around ${headers} to allow shell word splitting for multiple header arguments
    HTTP_RESPONSE=$(curl -s -X GET -w "%{http_code}\n%{raw_output}" ${headers} "${url}")
    HTTP_STATUS=$(echo "${HTTP_RESPONSE}" | head -n 1)
    HTTP_BODY=$(echo "${HTTP_RESPONSE}" | tail -n +2)

    if [ "${HTTP_STATUS}" -eq "${expected_status}" ]; then
        if [[ "${HTTP_BODY}" =~ ${expected_body_regex} ]]; then
            echo "SUCCESS"
            SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
        else
            echo "FAIL: Body mismatch. Expected regex '${expected_body_regex}', Got: '${HTTP_BODY}'"
        fi
    else
        echo "FAIL: Status mismatch. Expected ${expected_status}, Got: ${HTTP_STATUS}. Body: '${HTTP_BODY}'"
    fi
}

# 1. /headers
# Updated regex to explicitly match the expected headers from a standard curl GET request.
# The regex uses lookaheads and optional matching for order flexibility.
test_endpoint "${BASE_URL}/headers" 200 '\{"headers":\{.*"Accept":"\*\/\*",.*"Host":"localhost:8080",.*"User-Agent":"curl\/[0-9\.]+".*\}\}'

# 2. /user-agent
test_endpoint "${BASE_URL}/user-agent" 200 '"user-agent":'

# 3. /random-string
# Verify length is 32 alphanumeric chars.
TOTAL_TESTS=$((TOTAL_TESTS + 1))
echo -n "Testing ${BASE_URL}/random-string (Expected: 200, 32 alphanumeric chars)... "
RANDOM_STR_RESPONSE=$(curl -s -X GET -w "%{http_code}\n%{raw_output}" "${BASE_URL}/random-string")
RANDOM_STR_STATUS=$(echo "${RANDOM_STR_RESPONSE}" | head -n 1)
RANDOM_STR_BODY=$(echo "${RANDOM_STR_RESPONSE}" | tail -n +2)

if [ "${RANDOM_STR_STATUS}" -eq 200 ]; then
    if [[ "${#RANDOM_STR_BODY}" -eq 32 && "${RANDOM_STR_BODY}" =~ ^[a-zA-Z0-9]+$ ]]; then
        echo "SUCCESS"
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
        echo "FAIL: Body mismatch. Expected 32 alphanumeric chars, Got: '${RANDOM_STR_BODY}' (Length: ${#RANDOM_STR_BODY})"
    fi
else
    echo "FAIL: Status mismatch. Expected 200, Got: ${RANDOM_STR_STATUS}. Body: '${RANDOM_STR_BODY}'"
fi

# 4. /foo (without header)
test_endpoint "${BASE_URL}/foo" 403 "403 Forbidden: 'Test' header is required."

# 5. /foo (with header)
test_endpoint "${BASE_URL}/foo" 200 "\./bar" "-H \"Test: arbitrary-value\""

# 6. /bar (without header)
test_endpoint "${BASE_URL}/bar" 403 "403 Forbidden: 'Test' header is required."

# 7. /bar (with header)
test_endpoint "${BASE_URL}/bar" 200 "True" "-H \"Test: some-other-value\""

# 8. /nonexistent-file.dhall
test_endpoint "${BASE_URL}/nonexistent-file.dhall" 404 "404 Not Found"

# 9. /xyz
test_endpoint "${BASE_URL}/xyz" 200 "1234"

# 10. /static/hello.txt
test_endpoint "${BASE_URL}/static/hello.txt" 200 "Hello from the embedded static file! This is plain text content."

# --- Test Summary ---
cat << EOF
---
### Test Summary

Here's a summary of the test execution:

* **Total tests run:** ${TOTAL_TESTS}
* **Successful tests:** ${SUCCESS_COUNT}
* **Failed tests:** $((TOTAL_TESTS - SUCCESS_COUNT))

---
EOF

if [ "${SUCCESS_COUNT}" -eq "${TOTAL_TESTS}" ]; then
    echo "All tests passed successfully!"
    exit 0
else
    echo "Some tests failed!"
    exit 1
fi
