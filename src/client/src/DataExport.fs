module DataExport

open Api
open Api.DataExport
open Elmish
open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.DataExport

type ExportDatabase =
    | ExportDatabaseIdle
    | ExportingDatabase
    | ExportDatabaseFailed of ApiError<ExportDatabaseError>

type LoadedModel = {
    ExportDatabase: ExportDatabase
}

type Model =
    | NotLoaded
    | Loaded of AuthKey * LoadedModel

type Msg =
    | ExportDatabase
    | ExportDatabaseResult of Result<Browser.Types.Blob, ApiError<ExportDatabaseError>>
    | DownloadDatabaseResult

let init authKey =
    match authKey with
    | Some authKey ->
        let model =
            {
                ExportDatabase = ExportDatabaseIdle
            }
        Loaded (authKey, model), Cmd.none
    | None ->
        NotLoaded, Cmd.none

let private download fileName (blob: Browser.Types.Blob) =
    let link = Browser.Dom.document.createElement("a") :?> Browser.Types.HTMLAnchorElement
    link.href <- Browser.Dom.window?URL?createObjectURL(blob)
    link?download <- fileName
    link.click()

let update msg state =
    match msg, state with
    | ExportDatabase, Loaded (authKey, loadedModel) ->
        Loaded (authKey, { loadedModel with ExportDatabase = ExportingDatabase }),
        Cmd.OfAsync.perform exportDatabase authKey ExportDatabaseResult
    | ExportDatabase, NotLoaded -> state, Cmd.none
    | ExportDatabaseResult (Ok data), Loaded (authKey, loadedModel) ->
        let timestamp = System.DateTime.Now.ToString("yyyy-MM-dd-HH-mm-ss")
        Loaded (authKey, { loadedModel with ExportDatabase = ExportDatabaseIdle }),
        Cmd.OfFunc.perform (download $"data-%s{timestamp}.db") data (fun _ -> DownloadDatabaseResult)
    | ExportDatabaseResult (Error e), Loaded (authKey, loadedModel) ->
        Loaded (authKey, { loadedModel with ExportDatabase = ExportDatabaseFailed e }), Cmd.none
    | ExportDatabaseResult _, NotLoaded -> state, Cmd.none
    | DownloadDatabaseResult, _ -> state, Cmd.none

[<ReactComponent>]
let DataExport authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    React.useEffect(fun () ->
        match state with
        | Loaded (_, { ExportDatabase = ExportDatabaseFailed (ExpectedError ExportDatabaseError.InvalidAuthKey) })
        | Loaded (_, { ExportDatabase = ExportDatabaseFailed (ExpectedError ExportDatabaseError.NotAuthorized) }) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    match state with
    | NotLoaded -> Html.none
    | Loaded (_, loadedModel) ->
        React.fragment [
            Bulma.container [
                text.hasTextCentered
                prop.children [
                    Bulma.button.button [
                        match loadedModel.ExportDatabase with
                        | ExportingDatabase ->
                            prop.disabled true
                            button.isLoading
                        | _ -> ()
                        color.isLink
                        prop.text "Datenbank exportieren"
                        prop.onClick (fun _ -> dispatch ExportDatabase)
                    ]
                ]
            ]
        ]
