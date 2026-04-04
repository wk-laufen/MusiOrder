# AGENTS.md - MusiOrder Development Guide

## General rules

* Don't run any git operations that *modify* the history (commit, rebase, etc.) unless explicitely told to.
* Don't stage or unstage files unless explicitely told to.
* Don't clean the working tree or reset the current branch to a different commit unless explicitely told to.

## Project Overview

MusiOrder is an F# application with three components:
- **Client**: Fable (F# to JavaScript compiler) + React (Feliz), Tailwind CSS
- **Server**: ASP.NET Core with Giraffe, SQLite database
- **NFC Reader**: ASP.NET Core minimal web API, communicates with card readers

## Build/Lint/Test Commands

### Prerequisites
- .NET SDK 10.0 (client, server, nfc-reader)
- Node.js >= 18
- npm packages: `dotnet tool restore` must be run before building

### Client Development
```bash
cd src/client
npm install              # Install npm dependencies
dotnet tool restore      # Restore Fable toolchain
npm start                # Start dev server with hot reload (dotnet fable watch + vite)
npm run build            # Production build (dotnet fable + vite build)
npm run clean            # Clean build artifacts
```

### Server Development
```bash
cd src/server
dotnet watch run --environment Development --urls "http://+:5000"   # Dev server
DB_PATH=/path/to/db.sqlite dotnet run                              # With custom DB
```

### NFC Reader Development
```bash
cd src/nfc-reader
dotnet watch run --environment Development --urls "http://+:8080"
```

### VS Code Tasks
Press `Ctrl+Shift+B` and select:
- **Dev**: Runs all three components (server, NFC reader, client)
- **Run server**: Server only
- **Run client**: Client only
- **Run NFC reader**: NFC reader only

### Formatting
- **Fantomas**: Run after code changes (`fantomas <file>.fs`)
- Request formatting by mentioning "Fantomas" when files have been changed

### Running Tests
Tests exist in `src/server.tests/MusiOrder.Server.Tests.fsproj` (Expecto framework).

To run all tests after code changes:
```bash
cd src/server.tests && dotnet run
```

This will output test results like:
```
EXPECTO? Running tests...
EXPECTO! 63 tests run for MusiOrder.Server – all passed
```

## Code Style Guidelines

### General Principles
- F# is the primary language for all components
- Use functional programming patterns (immutability, pattern matching, pipeline operators)
- Prefer discriminated unions over exception-based error handling
- Keep functions small and focused

### Formatting
- **Indentation**: 4 spaces (F#) or 2 spaces (JS interop code)
- **Line length**: Soft limit 100 characters
- **No trailing whitespace**
- **No semicolons** in F# code
- **String interpolation**: Use `$"..."` syntax
- **Pipeline operator**: Prefer `|>` for function composition

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Modules | PascalCase | `module OrderAdministration` |
| Types | PascalCase | `type UserInfo`, `type PositiveInteger` |
| Type aliases | PascalCase | `type UserId = UserId of string` |
| Record fields | PascalCase | `{ Id: UserId; FirstName: string }` |
| Discriminated union cases | PascalCase | `InvalidAuthKey`, `NotAuthorized` |
| Functions | camelCase | `let getUserInfo () = ...` |
| Private helpers | camelCase | `let private isDuplicateKeyException` |
| Parameters | camelCase | `let saveOrder (userId: UserId) = ...` |
| Module methods | camelCase | `module PositiveInteger` with `tryCreate`, `value` |
| Constants | PascalCase | N/A (avoid in F#) |

### File Organization
- One module per file (file name matches module name)
- Files included in `.fsproj` in dependency order
- Shared types in `src/shared/`
- Client-specific code in `src/client/src/`
- Server-specific code in `src/server/`

### Import Order (F#)
```fsharp
namespace MusiOrder.Models

open System
#if FABLE_COMPILER
open Thoth.Json        // Fable-specific imports
#else
open Thoth.Json.Net    // .NET-specific imports
#endif
open OtherMusiOrder.Modules  // Project imports
```

### Domain Modeling Patterns

**Type Wrappers** (for domain primitives):
```fsharp
type PositiveInteger = private PositiveInteger of int
module PositiveInteger =
    let tryCreate = function
        | v when v > 0 -> Some (PositiveInteger v)
        | _ -> None
    let value (PositiveInteger v) = v
```

**Discriminated Unions for Errors**:
```fsharp
type AddOrderError =
    | InvalidAuthKey
    | NotAuthorized
    | NoOrderUser
    | OrderEntryErrors of ProductId * OrderEntryError list
```

**Auto-Open Modules** (for utilities):
```fsharp
[<AutoOpen>]
module Utils

let uncurry fn (a, b) = fn a b
```

### React/Feliz Guidelines

**Component Definition**:
```fsharp
[<ReactComponent>]
let MyComponent () =
    let (state, dispatch) = React.useElmish(init, update, [||])
    // ... render code using Html.* functions
```

**JS Interop**:
```fsharp
[<Emit("navigator.language")>]
let language : string = jsNative

[<Import("setupKeyboards", "./on-screen-keyboard.js")>]
let setupKeyboards : unit -> unit = jsNative
```

**Styling**: Use Tailwind CSS classes via `prop.className` or `prop.classes`:
```fsharp
Html.div [
    prop.className "flex flex-col items-center px-4 py-2"
    // ...
]
```

### Error Handling

- **Prefer Result types** over exceptions for expected failures
- **Use try/with** sparingly, only for truly exceptional cases
- **Discriminated unions** for domain errors (see patterns above)
- **Option types** for nullable values
- **TaskBuilder.fs** for async/await (`task { ... }`)

### Database Patterns (Server)

- Use `DB.read`, `DB.write`, `DB.readSingle` helpers
- Always use parameterized queries (SQL injection prevention)
- Use backticks for SQLite identifiers: `` `columnName` ``
- Balance stored as integers (cents) to avoid floating-point issues

```fsharp
let getUserBalance (UserId userId) = task {
    let! totalOrderPrice = DB.readSingle 
        "SELECT SUM(...) FROM `Order` WHERE userId = @UserId" 
        [ ("@UserId", userId) ] 
        (fun reader -> reader.GetDecimal(0))
    return totalOrderPrice / 100m
}
```

### Key Libraries

| Layer | Library | Notes |
|-------|---------|-------|
| Client UI | Feliz | F# wrapper for React |
| Client State | Elmish | Model-View-Update pattern |
| Client HTTP | Fable.Fetch, Thoth.Fetch | API calls |
| Client JSON | Thoth.Json | JSON serialization |
| Server HTTP | Giraffe | F# web framework |
| Server JSON | Thoth.Json.Net | JSON serialization |
| Database | Microsoft.Data.Sqlite | SQLite access |
| CSS | Tailwind CSS 4 | Utility-first CSS |
| Icons | Font Awesome | Via CDN/CSS import |

### Common Tasks

**Adding a new API endpoint**:
1. Define types in `src/shared/Models.fs`
2. Add encoder/decoder in `Json` module
3. Implement logic in `src/server/Core.fs`
4. Add route in `src/server/Program.fs` (Giraffe router)
5. Add client API function in `src/client/src/Api.fs`
6. Consume in client components

**Adding a new domain type**:
1. Create type wrapper with private constructor
2. Add module with `tryCreate`, `value`, `encode`, `decoder`
3. Register encoder/decoder in `Json.coders`
4. Use throughout codebase

**Modifying database schema**:
1. Update `db-schema.sql`
2. Update `DB.fs` helper functions if needed
3. Test with sample database

### Development Tips

- Use `dotnet fable clean src --yes` to clear Fable cache when having compilation issues
- Set `DB_PATH` environment variable to point to test database
- Client proxy in `vite.config.mjs` forwards `/api` to server on port 5000
- NFC reader URL can be set in browser localStorage: key `nfc-reader-url`
