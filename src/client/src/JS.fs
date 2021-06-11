module JS

open Fable.Core

let [<Import("*","moment")>] moment: System.DateTimeOffset -> obj = jsNative
