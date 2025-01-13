module UserCards

open Elmish
open Feliz
open Feliz.UseElmish
open MusiOrder.Models.Order

type Msg =
    | SetUserFilter of char option option

type State = {
    Users: UserInfo list
    Filter: char option option
}

let update msg state =
    match msg with
    | SetUserFilter filter ->
        { state with Filter = filter }, Cmd.none

let init users = { Users = users; Filter = None}, Cmd.none

[<ReactComponent>]
let UserCards (users: UserInfo list) onClick =
    let (state, dispatch) = React.useElmish(init users, update, [||])

    Html.div [
        prop.className "flex flex-col gap-2"
        prop.children [
            Html.div [
                prop.className "flex flex-wrap justify-center gap-2"
                prop.children [
                    let groups = [
                        ("Alle", None)
                        yield! users
                            |> List.map (fun v -> v.LastName |> Seq.tryHead |> Option.map System.Char.ToUpper)
                            |> List.distinct
                            |> List.sort
                            |> List.map (fun v -> ($"%c{v |> Option.defaultValue '␣'}", Some v))
                    ]
                    yield!
                        groups
                        |> List.map (fun (title, filter) ->
                            Html.button [
                                prop.classes [
                                    "btn"
                                    if state.Filter = filter then "btn-solid btn-blue"
                                ]
                                prop.onClick (fun _ -> dispatch (SetUserFilter filter))
                                prop.text title
                            ]
                        )
                ]
            ]
            let users =
                state.Users
                |> List.filter (fun v ->
                    match state.Filter with
                    | Some firstChar -> v.LastName |> Seq.tryHead |> Option.map System.Char.ToUpper = firstChar
                    | None -> true
                )
                |> List.groupBy (fun v -> v.LastName |> Seq.tryHead |> Option.map System.Char.ToUpper)
            for (key, users) in users do
                Html.div [
                    prop.className "flex flex-col gap-2"
                    prop.children [
                        Html.div [
                            prop.className "flex gap-2 items-center"
                            prop.children [
                                match key with
                                | Some c ->
                                    Html.hr [ prop.className "grow h-px bg-gray-300" ]
                                    Html.span $"%c{c}"
                                    Html.hr [ prop.className "grow h-px bg-gray-300" ]
                                | None ->
                                    Html.hr [ prop.className "grow h-px bg-gray-300" ]
                            ]
                        ]
                        Html.div [
                            prop.className "flex flex-wrap justify-center gap-2"
                            prop.children [
                                for user in users do
                                    Html.div [
                                        prop.className "flex flex-col shadow rounded p-2 cursor-pointer"
                                        prop.onClick (fun _ -> onClick user)
                                        prop.children [
                                            Html.span [
                                                prop.className "text-center"
                                                prop.text $"%s{user.LastName.ToUpper()} %s{user.FirstName}"
                                            ]
                                            Html.span [
                                                prop.className $"text-center text-sm %s{View.balanceColor user.Balance}"
                                                prop.text (View.formatBalance user.Balance)
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    ]
                ]
        ]
    ]
