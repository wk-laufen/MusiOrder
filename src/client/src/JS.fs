module JS

open Fable.Core

let [<Import("default","moment")>] moment: System.DateTimeOffset -> obj = jsNative
