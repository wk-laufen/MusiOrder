module View

open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.Form.Simple
open Feliz
open global.JS

[<Emit("new Intl.NumberFormat(navigator.language, { style: 'currency', currency: 'EUR' }).format($0)")>]
let formatPrice (v: decimal) : string = jsNative

[<Emit("new Intl.NumberFormat(navigator.language, { minimumFractionDigits: 2 }).format($0)")>]
let formatNumber (v: decimal) : string = jsNative

[<Emit("new Intl.NumberFormat(navigator.language, { style: 'currency', currency: 'EUR', signDisplay: 'always' }).format($0)")>]
let formatBalance (v: decimal) : string = jsNative

let retryButton onClick =
    Html.button [
        prop.className "!flex items-center gap-2 btn btn-solid btn-green"
        prop.onClick (ignore >> onClick)
        prop.children [
            Fa.i [ Fa.Solid.SyncAlt ] []
            Html.span [ prop.text "Wiederholen" ]
        ]
    ]

let errorNotificationWithRetry (message: string) onRetry =
    Html.div [
        prop.className "flex items-center gap-4 bg-musi-red rounded text-white px-8 py-4"
        prop.children [
            Fa.i [ Fa.Solid.ExclamationTriangle; Fa.Size Fa.Fa2x ] []
            Html.span [
                prop.className "text-2xl"
                prop.text message
            ]
            retryButton onRetry
        ]
    ]

let infoNotification (message: string) (elements: ReactElement list) =
    Html.div [
        prop.className "flex items-center gap-4 bg-musi-blue rounded px-8 py-4 text-white"
        prop.children [
            Html.span [
                prop.className "text-2xl"
                prop.text message
            ]
            yield! elements
        ]
    ]

let loadIconBig =
    Html.div [
        prop.className "text-center text-musi-gold py-2"
        prop.children [
            Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
        ]
    ]

let modal (title: string) onHide (body: ReactElement list) (footer: ReactElement list) =
    ReactDOM.createPortal (
        Html.div [
            prop.className "fixed inset-0 grid h-screen w-screen place-items-center bg-slate-800 bg-opacity-60 backdrop-blur-sm animate-fadeIn"
            prop.onClick (ignore >> onHide)
            prop.children [
                Html.div [
                    prop.className "min-w-[640px] max-h-screen px-8 py-8 flex flex-col"
                    prop.onClick (fun e -> e.stopPropagation())
                    prop.children [
                        Html.div [
                            prop.className "grow-0 bg-slate-200 rounded-t-lg px-8 py-4 text-2xl"
                            prop.text title
                        ]
                        Html.div [
                            prop.className "overflow-y-auto grow-1 p-8 bg-white"
                            prop.children body
                        ]
                        Html.div [
                            prop.className "grow-0 flex justify-end gap-2 bg-slate-200 text-white rounded-b-lg px-4 py-2"
                            prop.children [
                                yield! footer
                                Html.button [
                                    prop.className "btn btn-white"
                                    prop.onClick (ignore >> onHide)
                                    prop.text "Schließen"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ],
        Browser.Dom.document.body
    )

let authForm =
    Html.div [
        prop.className "flex flex-col gap-2 items-center text-musi-gold"
        prop.children [
            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
            Html.span [
                prop.className "text-center text-3xl"
                prop.children [
                    Html.text "Authentifiziere dich mit deinem"
                    Html.br []
                    Html.text "Musischlüssel"
                ]
            ]
        ]
    ]

let modalAuthForm title onHide =
    modal title onHide [ authForm ] []

let authError error onRetry =
    match error with
    | React.ReaderNotAvailable -> 
        errorNotificationWithRetry "Lesegerät nicht erkannt." onRetry

let modalAuthError title error onRetry onHide =
    modal title onHide [ authError error onRetry ] []

let balanceColor balance =
    if balance >= 5.m then "text-musi-green"
    elif balance >= 0.m then "text-musi-blue"
    else "text-musi-red"

let form title fields data dispatch closeMsg formChangedMsg =
    let formBuilder = fun (config: Form.View.FormConfig<_>) ->
        modal title (fun () -> dispatch closeMsg)
            [
                Html.form [
                    prop.id "modal-form"
                    prop.onSubmit (fun ev ->
                        ev.preventDefault()

                        config.OnSubmit
                        |> Option.map dispatch
                        |> Option.defaultWith ignore
                    )
                    prop.children config.Fields
                ]
            ]
            [
                match config.State with
                | Form.View.Error message ->
                    Html.span [
                        prop.className "self-center text-musi-red"
                        prop.text message
                    ]
                | Form.View.Success message ->
                    Html.span [
                        prop.className "self-center text-musi-green"
                        prop.text message
                    ]
                | Form.View.Loading
                | Form.View.Idle -> ()

                Html.button [
                    prop.form "modal-form"
                    prop.className "btn btn-solid btn-green"
                    prop.text config.Action
                    prop.disabled (config.State = Form.View.Loading)
                ]
            ]

    let wrapFormField (label: string) showError error (field: ReactElement) =
        Html.div [
            prop.className "flex flex-col gap-2 mb-4 last:mb-0"
            prop.children [
                Html.span [
                    prop.className "font-semibold"
                    prop.text label
                ]
                field
                if showError then
                    Html.span [
                        prop.className "text-sm text-musi-red"
                        match error with
                        | Some error ->
                            let message =
                                match error with
                                | Fable.Form.Error.RequiredFieldIsEmpty -> $"%s{label} darf nicht leer sein."
                                | Fable.Form.Error.ValidationFailed message -> message
                                | Fable.Form.Error.External message -> message
                            prop.text message
                        | _ -> ()
                    ]
            ]
        ]

    let inputFieldBuilder typeName = fun (config: Form.View.TextFieldConfig<_>) ->
        Html.input [
            // dirty hack to change HTML attributes
            let typeName = Option.ofObj config.Attributes?Type |> Option.defaultValue typeName
            prop.type' typeName
            yield! Option.ofObj config.Attributes?Min |> Option.map (fun v -> prop.custom ("min", string v)) |> Option.toList
            yield! Option.ofObj config.Attributes?Max |> Option.map (fun v -> prop.custom ("max", string v)) |> Option.toList
            yield! Option.ofObj config.Attributes?Step |> Option.map (fun v -> prop.custom ("step", string v)) |> Option.toList
            prop.classes [
                if config.ShowError && config.Error.IsSome then "border-musi-red"
            ]
            prop.onChange (fun (text : string) -> config.OnChange text |> dispatch)
            match config.OnBlur with
            | Some onBlur -> prop.onBlur (fun _ -> dispatch onBlur)
            | None -> ()
            prop.disabled config.Disabled
            prop.placeholder config.Attributes.Placeholder
            prop.value config.Value
        ]
        |> wrapFormField config.Attributes.Label config.ShowError config.Error

    let radioFieldBuilder = fun (config: Form.View.RadioFieldConfig<_>) ->
        Html.div [
            prop.className "flex gap-4"
            prop.children [
                yield!
                    config.Attributes.Options
                    |> List.map (fun (value, title) ->
                        Html.label [
                            prop.className "inline-flex items-center gap-2"
                            prop.children [
                                Html.input [
                                    prop.type' "radio"
                                    prop.value value
                                    prop.name config.Attributes.Label
                                    prop.isChecked (config.Value = value)
                                    prop.disabled config.Disabled
                                    prop.onChange (fun (_ : bool) -> config.OnChange value |> dispatch)
                                    match config.OnBlur with
                                    | Some onBlur -> prop.onBlur (fun _ -> dispatch onBlur)
                                    | None -> ()
                                ]
                                Html.span [
                                    prop.text title
                                ]
                            ]
                        ]
                    )
            ]
        ]
        |> wrapFormField config.Attributes.Label config.ShowError config.Error

    let selectFieldBuilder = fun (config: Form.View.SelectFieldConfig<_>) ->
        Html.select [
            prop.classes [
                if config.ShowError && config.Error.IsSome then "border-musi-red"
            ]
            prop.onChange (fun (value : string) -> config.OnChange value |> dispatch)
            match config.OnBlur with
            | Some onBlur -> prop.onBlur (fun _ -> dispatch onBlur)
            | None -> ()
            prop.disabled config.Disabled
            prop.placeholder config.Attributes.Placeholder
            prop.value config.Value
            prop.children [
                Html.option [
                    prop.disabled true
                    prop.value ""
                    prop.text ($"-- {config.Attributes.Placeholder} --")
                ]
                for (value, text) in config.Attributes.Options do
                    Html.option [
                        prop.value value
                        prop.text text
                    ]
            ]
        ]
        |> wrapFormField config.Attributes.Label config.ShowError config.Error

    let htmlViewConfig : Form.View.CustomConfig<_> = {
        Form = formBuilder
        TextField = inputFieldBuilder "text"
        PasswordField = inputFieldBuilder "password"
        EmailField = fun config -> failwith "EmailField not implemented"
        TextAreaField = fun config -> failwith "TextAreaField not implemented"
        CheckboxField = fun config -> failwith "CheckboxField not implemented"
        RadioField = radioFieldBuilder
        SelectField = selectFieldBuilder
        Group = fun fields -> failwith "Group not implemented"
        Section = fun title fields -> failwith "Section not implemented"
        FormList = fun config -> failwith "FormList not implemented"
        FormListItem = fun config -> failwith "FormListItem not implemented"
    }
    let config: Form.View.ViewConfig<_, _> =
        {
            Dispatch = dispatch
            OnChange = formChangedMsg
            Action = "Speichern"
            Validation = Form.View.ValidateOnSubmit
        }
    Form.View.custom htmlViewConfig config fields data

module Order =
    open MusiOrder.Models.Order

    let orderSummary (orderSummary: OrderSummary) =
        Html.div [
            prop.className "flex flex-col gap-2 text-center"
            prop.children [
                Html.span [
                    Html.text "Dein aktuelles Guthaben beträgt: "
                    Html.span [
                        prop.className $"text-lg %s{balanceColor orderSummary.Balance}"
                        prop.text (formatBalance orderSummary.Balance)
                    ]
                ]
                Html.div [
                    prop.className "flex flex-col gap-2"
                    prop.children [
                        match orderSummary.LatestOrders with
                        | [] -> Html.text "Keine Bestellungen in der letzten Zeit. 😱"
                        | orders ->
                            Html.span [ prop.text "Deine letzten Bestellungen waren:" ]
                            Html.ul [
                                prop.className "flex flex-col gap-2"
                                prop.children [
                                    for order in orders do
                                        Html.li [
                                            Html.textf "%s: " (moment(order.Timestamp)?fromNow())
                                            Html.span [
                                                prop.className "inline-block px-2 py-1 bg-musi-blue text-white rounded"
                                                prop.text $"%d{order.Amount} x %s{order.ProductName}"
                                            ]
                                        ]
                                ]
                            ]
                    ]
                ]
            ]
        ]
