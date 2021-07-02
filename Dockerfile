FROM mcr.microsoft.com/dotnet/sdk:5.0 AS client-build-env

# Install Node.js and yarn
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && \
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt update && \
    apt install -y nodejs yarn

RUN echo "Node version: $(node --version)" && \
    echo "Yarn version: $(yarn --version)"

WORKDIR /app

COPY ./.config ./.config
RUN dotnet tool restore

# Copy fsproj and restore as distinct layers
COPY ./src/client/src/*.fsproj ./client/src/
RUN dotnet restore ./client/src/

# Copy package.json and restore as distinct layers
COPY ./src/client/package.json ./src/client/yarn.lock ./client/
RUN yarn install --frozen-lockfile --cwd ./client

# Copy everything else and build
COPY ./src/client ./client
COPY ./src/shared ./shared

WORKDIR /app/client
RUN dotnet fable ./src --run yarn --cwd ./src build

###

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS server-build-env

WORKDIR /app

# Copy fsproj and restore as distinct layers
COPY ./src/server/*.fsproj ./server/
RUN dotnet restore ./server

# Copy everything else and build
COPY ./src/server ./server
COPY ./src/shared ./shared
RUN dotnet publish ./server -c Release -o out

###

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:5.0
WORKDIR /app
COPY --from=server-build-env /app/out .
COPY --from=client-build-env /app/client/deploy ./wwwroot
ENTRYPOINT ["dotnet", "MusiOrder.Server.App.dll"]
