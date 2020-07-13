module JS

open Fable.Core
open Fable.Core.JsInterop

let [<Import("*","moment")>] moment: System.DateTimeOffset -> obj = jsNative
moment?locale("de-AT")
