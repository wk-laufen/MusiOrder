$env:ASPNETCORE_ENVIRONMENT = "Development"
$env:DB_PATH = "$pwd\data\musiorder.db"
dotnet watch --project .\src\server run --urls "http://+:5000"
