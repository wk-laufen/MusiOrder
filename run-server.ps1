$env:ASPNETCORE_ENVIRONMENT = "Development"
$env:DB_PATH = "$pwd\musiorder.db"
dotnet watch --project .\src\server run
