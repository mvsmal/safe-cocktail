module Client

open Fable.Helpers.React
open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

open Fable.MaterialUI.Core
open Fable.MaterialUI.Themes
open Fable.MaterialUI.Props
open Fable.MaterialUI
open Shared
open Components
open Types


module Server =

    open Fable.Remoting.Client

    let api : CocktailApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<CocktailApi>

let pageParser : Parser<Page->Page,Page> =
    oneOf [
        map Page.Search (s "")
        map Page.Search (s "search")
        map Page.Cocktail (s "cocktail" </> i32)
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        model,Navigation.newUrl (toHash Page.Search)
    | Some page ->
        let cmd = function
            | Page.Cocktail id -> Cmd.ofMsg (LoadCocktailDetails id)
            | _ -> Cmd.none

        { model with currentPage = page }, cmd page

let init result =
    let (model, cmd) =
        urlUpdate result
            { currentPage = Page.Search
              keyword = ""
              cocktails = []
              cocktailDetails = None
              isLoading = false }

    model, cmd

let searchCocktail keyword =
    async {
        match! Server.api.search keyword with
        | Ok cocktails -> return cocktails
        | Result.Error e -> return failwith e
    }

let loadCocktailDetails id =
    async {
        match! Server.api.details id with
        | Ok details -> return details
        | Result.Error e -> return failwith e
    }

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | Search ->
        currentModel, Cmd.batch [ Cmd.ofAsync searchCocktail currentModel.keyword SearchSuccess Failure
                                  Cmd.ofMsg (Load true) ]
    | KeywordChanged keyword ->
        { currentModel with keyword = keyword }, Cmd.none
    | SearchSuccess cocktails ->
        { currentModel with cocktails = cocktails }, Cmd.ofMsg (Load false)
    | LoadCocktailDetails id ->
        currentModel, Cmd.batch [ Cmd.ofAsync loadCocktailDetails id CocktailDetailsSuccess Failure
                                  Cmd.ofMsg (Load true) ]
    | CocktailDetailsSuccess details ->
        { currentModel with cocktailDetails = details }, Cmd.ofMsg (Load false)
    | Failure e ->
        currentModel, Cmd.batch [ Toast.error e.Message
                                  Cmd.ofMsg (Load false) ]
    | Load isLoading ->
        { currentModel with isLoading = isLoading }, []

let theme =
    createMuiTheme [
        Typography [ UseNextVariants true ]
        ThemeProp.Palette [
            PaletteProp.Primary [ PaletteIntentionProp.Main Colors.grey.``900`` ]
            PaletteProp.Secondary [ PaletteIntentionProp.Main Colors.yellow.A200 ]
        ]
    ]

let searchPage model dispatch =
    fragment [] [
        searchBox model dispatch
        searchResults model dispatch
    ]

let detailsPage model dispatch =
    fragment [] [
        detailsCard model dispatch
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    let pageHtml = function
        | Page.Search -> searchPage model dispatch
        | Page.Cocktail _ -> detailsPage model dispatch
    muiThemeProvider [MuiThemeProviderProp.Theme (ProviderTheme.Theme theme) ] [
        div [] [
            myAppBar () ignore
            pageHtml model.currentPage
        ]
    ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
