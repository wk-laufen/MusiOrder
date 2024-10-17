$env:DB_PATH = "$pwd\data\data.db"
dotnet watch --project .\src\server run --urls "http://+:5000" --environment Development
