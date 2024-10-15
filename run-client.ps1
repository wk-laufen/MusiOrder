$env:NODE_OPTIONS = "--openssl-legacy-provider"
dotnet fable watch .\src\client\src --run yarn --cwd .\src\client start
