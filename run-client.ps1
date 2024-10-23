dotnet tool restore
Push-Location .\src\client
yarn install --frozen-lockfile
Pop-Location
$env:NODE_OPTIONS = "--openssl-legacy-provider"
dotnet fable watch .\src\client\src --run yarn --cwd .\src\client start
