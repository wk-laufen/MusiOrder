#!/bin/bash

pushd .. > /dev/null

dotnet publish ./src/server --runtime linux-arm --self-contained -c Release -o ./out/app
npm run build --prefix ./src/client/
mv ./src/client/dist/ ./out/app/wwwroot

dotnet publish ./src/nfc-reader --runtime linux-arm --self-contained -c Release -o ./out/nfc-reader

tar -czvf out.tar.gz -C ./out .

popd > /dev/null
