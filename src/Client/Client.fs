module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop



open Fable.MaterialUI.Core
open Fable.MaterialUI.Themes
open Fable.MaterialUI.Props
open Fable.MaterialUI
open Shared
open Fable.Import.React
open Components
open Types


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ICounterApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<ICounterApi>

let initialCounter = Server.api.initialCounter

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { keyword = "" }

    initialModel, []



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | Search -> currentModel, []
    | KeywordChanged keyword ->
        { currentModel with keyword = keyword }, []

let theme =
    createMuiTheme [
        Typography [
            UseNextVariants true
        ]
        ThemeProp.Palette [
            PaletteProp.Primary [
                PaletteIntentionProp.Main Colors.grey.``900``
            ]
            PaletteProp.Secondary [
                PaletteIntentionProp.Main Colors.yellow.A200
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    muiThemeProvider [MuiThemeProviderProp.Theme (ProviderTheme.Theme theme) ] [
        div [] [
            myAppBar () ignore
            searchBox model dispatch
        ]
    ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
