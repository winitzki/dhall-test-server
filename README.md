# Dhall test server

The goal of this small project is to prevent Dhall standard tests from depending on external Web services.
That dependence has been a bottleneck for PR running and even for local development.

For instance, I have observed that `httpbin.org`, `tests.dhall-lang.org`, or `prelude.dhall-lang.org` sometimes gets overloaded with requests and becomes unavailable.
This leads to test failures in CI or on the local machine, although there is no fault in the code.
Especially, `httpbin.org` regularly responds with `503 Service Temporarily Unavailable`.

This project will implement a local web server that responds to all endpoints needed by the Dhall tests.
Eventually, this code might be merged into one of the dhall repositories.

The [Dhall language](https://dhall-lang.org) has standard tests that depend on the following Web URLs:

- `https://test.dhall-lang.org/random-string`  - must return a random string such as `3vV6UjwSy0E5vOjHPaBuqrRAgC826aeM`
- `https://test.dhall-lang.org/foo`  - must return 403 unless the `Test` header is given with arbitrary value, and if so, returns `./bar`
- `https://test.dhall-lang.org/bar` - must return 403 unless the `Test` header is given with arbitrary value, and if so, returns `True`
- `https://test.dhall-lang.org/foo/../random-string`  - must return the same as /random-string
- `https://test.dhall-lang.org/nonexistent-file.dhall` - must return 404 status with text "404: Not Found"
- `https://test.dhall-lang.org/cors/AllowedAll.dhall`
- `https://test.dhall-lang.org/cors/Empty.dhall`
- `https://test.dhall-lang.org/cors/NoCORS.dhall`
- `https://test.dhall-lang.org/cors/Null.dhall`
- `https://test.dhall-lang.org/cors/OnlyGithub.dhall`
- `https://test.dhall-lang.org/cors/OnlyOther.dhall`
- `https://test.dhall-lang.org/cors/OnlySelf.dhall`
- `https://test.dhall-lang.org/cors/SelfImportAbsolute.dhall`
- `https://test.dhall-lang.org/cors/SelfImportRelative.dhall`
- `https://test.dhall-lang.org/cors/TwoHopsFail.dhall`
- `https://test.dhall-lang.org/cors/TwoHopsSuccess.dhall`

- `httpbin.org/user-agent`  - Must return the value of the user-agent header as JSON, for example: `{ "user-agent": "blah" }`
- `httpbin.org/headers` - Must return all headers as JSON, for example: `{ "headers" : { "Host": "httpbin.org", "User-Agent": "blah" } }`

The following URLs just need to support GET requests for static, publicly available resources without any special headers or authentication:

- `https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/asLocation/Canonicalize3A.dhall`
- `https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/asLocation/Canonicalize5A.dhall`
- `https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/asLocation/EnvA.dhall`
- `https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/asLocation/MissingA.dhall`
- `https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/bar/import.dhall`
- `https://prelude.dhall-lang.org/List/length`
- `https://prelude.dhall-lang.org/package.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/0b983b92aa2222dc3e292c20550ee37dea3f41df/tests/import/data/example.txt`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/0b983b92aa2222dc3e292c20550ee37dea3f41df/tests/import/data/simple.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/0b983b92aa2222dc3e292c20550ee37dea3f41df/tests/import/data/simpleLocation.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/AllowedAll.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/Empty.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/NoCORS.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/Null.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/OnlyGithub.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/OnlyOther.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/OnlySelf.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/SelfImportAbsolute.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/SelfImportRelative.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/acee30866a179c9e9bb3fc02ec8be2883685eb14/tests/import/data/cors/Prelude.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/tests/import/data/referentiallyOpaque.dhall`
- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/tests/import/success/customHeadersA.dhall`
