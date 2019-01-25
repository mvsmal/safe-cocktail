module Components
open Fable.MaterialUI.Core
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Fable.MaterialUI.Themes
open Fable.MaterialUI.Core
open Fable.MaterialUI.Props
open Fable.Import.React
open Types
open Shared

type GlobalProps<'model, 'msg> =
    abstract model : 'model with get, set
    abstract dispatch : Dispatch<'msg> with get, set
    inherit IClassesProps

let private styled<'model,'msg> (c : ('model -> Dispatch<'msg> -> IClasses -> ReactElement)) (s : (ITheme -> IStyles list)) =
    let comp (props : GlobalProps<'model, 'msg>) =
        c props.model props.dispatch props.classes
    let compWithStyles = withStyles (StyleType.Func s) [] comp

    let result (m : 'model) (d : Dispatch<'msg>) =
        let props = createEmpty<GlobalProps<'model, 'msg>>
        props.model <- m
        props.dispatch <- d
        from compWithStyles props []
    result

let secondaryH6 s =
    typography [
        TypographyProp.Variant TypographyVariant.H6
        MaterialProp.Color ComponentColor.Secondary
    ] [ str s ]

let h6 s =
    typography [ TypographyProp.Variant TypographyVariant.H6 ] [ str s ]

let subtitle1 s =
    typography [ TypographyProp.Variant TypographyVariant.Subtitle1 ] [ str s ]


let private appBarStyles (_ : ITheme) : IStyles list =
    [
        Styles.Root [ FlexGrow 1 ]
        Styles.Custom ("middle", [ FlexGrow 1 ]) ]

let private myAppBar' _ _ classes =
    div [ Class !!classes?root ] [
        appBar [ AppBarProp.Position AppBarPosition.Static ] [
            toolbar [] [
                secondaryH6 "SAFE Cocktail Search"
                div [ Class !!classes?middle ] []
                button [
                    ButtonProp.Variant ButtonVariant.Contained
                    MaterialProp.Color ComponentColor.Secondary
                    MaterialProp.Component (!!"a")
                    HTMLAttr.Href (Page.Search |> toHash)
                ] [ str "Home" ]
            ]
        ]
    ]

let myAppBar = styled<unit,obj> myAppBar' appBarStyles


let private searchBoxStyles (theme : ITheme) : IStyles list =
    [ Styles.Root [
        CSSProp.MaxWidth 900
        CSSProp.Padding (theme.spacing.unit * 2)
        CSSProp.Margin (sprintf "%dpx auto" (theme.spacing.unit * 4))
      ]
      Styles.Form [
        CSSProp.Display "flex"
        CSSProp.AlignItems "center"
      ]
      Styles.Input [
        CSSProp.FlexGrow 1
        CSSProp.MarginRight (theme.spacing.unit * 2)
      ]
      ]

let private searchBox' model dispatch classes =
    paper [ Class !!classes?root ] [
        form [
            DOMAttr.OnSubmit (fun e -> e.preventDefault(); Search |> dispatch)
            Class !!classes?form
        ] [
            textField [
                Class !!classes?input
                MaterialProp.Margin FormControlMargin.Dense
                HTMLAttr.Placeholder "Enter cocktail name"
                TextFieldProp.Variant TextFieldVariant.Outlined
                HTMLAttr.Value model.keyword
                DOMAttr.OnChange (fun e -> e.Value |> KeywordChanged |> dispatch)
            ] []
            button [
                ButtonProp.Variant ButtonVariant.Contained
                MaterialProp.Color ComponentColor.Primary
                ButtonProp.Size ButtonSize.Large
                HTMLAttr.Type "submit"
            ] [ str "search" ]
        ]
    ]

let searchBox = styled searchBox' searchBoxStyles

let private searchResultStyles (theme : ITheme) : IStyles list =
    [
        Styles.Custom ("card", [ CSSProp.MaxWidth 280 ])
        Styles.Media [
            CSSProp.Height 0
            CSSProp.Padding "50%"
            CSSProp.BackgroundSize "cover"
        ]
    ]

let private searchResult' (model : Cocktail) _ classes =
    card [
        Class !!classes?card
        CardProp.Raised true
    ] [
        cardActionArea [
            HTMLAttr.Href (model.Id |> Page.Cocktail |> toHash)
        ] [
            cardHeader [
                HTMLAttr.Title model.Name
            ] []
            cardMedia [
                Class !!classes?media
                CardMediaProp.Image model.Image
                HTMLAttr.Title model.Name
            ]
        ]
    ]

let searchResult = styled<_,Msg> searchResult' searchResultStyles

let private searchResultsStyles (theme : ITheme) : IStyles list =
    [
        Styles.Root [
            CSSProp.MaxWidth 900
            CSSProp.Margin (sprintf "%dpx auto" (theme.spacing.unit * 4))
            CSSProp.TextAlign "center"
        ]
    ]

let private searchResults' (model : Model) dispatch classes =
    let results =
        match model.isLoading, model.cocktails with
        | true, _ -> div [] []
        | false, [] -> secondaryH6 "No cocktails found"
        | false, items ->
            gridList [
                HTMLAttr.Cols 3
                GridListProp.Spacing 20
                GridListProp.CellHeight (378 |> U2.Case1)
            ] (items |> List.map (fun c ->
                            gridListTile [ HTMLAttr.Cols 1 ] [ searchResult c dispatch ]))

    div [ Class !!classes?root ] [ results ]

let searchResults = styled<_,Msg> searchResults' searchResultsStyles


let private detailsStyles (theme : ITheme) : IStyles list =
    let centered = CSSProp.Margin (sprintf "%dpx auto" (theme.spacing.unit * 4))
    [
        Styles.Custom
            ("empty", [
                CSSProp.MaxWidth 900
                CSSProp.TextAlign "center"
                centered
            ])
        Styles.Root [
            CSSProp.MaxWidth 450
            centered
        ]
        Styles.Media [
            CSSProp.Height 0
            CSSProp.Padding "50%"
        ]
    ]

let private detailsCard' (model : Model) dispatch classes =
    match model.isLoading, model.cocktailDetails with
    | true, _ -> div [] []
    | false, None -> div [ Class !!classes?empty ] [ secondaryH6 "Not found" ]
    | false, Some cocktail ->
        card [
            Class !!classes?root
            CardProp.Raised true
        ] [
            cardHeader [
                HTMLAttr.Title cocktail.Name
                CardHeaderProp.Subheader !!cocktail.Category
            ] []
            cardMedia [
                Class !!classes?media
                CardMediaProp.Image cocktail.Image
                HTMLAttr.Title cocktail.Name
            ]
            cardContent [] [
                h6 cocktail.Alchogolic
                subtitle1 cocktail.Glass
                typography [ MaterialProp.Component !!"p" ] [ str cocktail.Instructions ]
                list [] (cocktail.Ingredients |> List.map (fun i ->
                    listItem [] [
                        avatar [ HTMLAttr.Src i.Image ] []
                        listItemText [
                            ListItemTextProp.Primary !!i.Name
                            ListItemTextProp.Secondary !!i.Measure
                        ] []
                    ]))
            ]
        ]

let detailsCard = styled<_,Msg> detailsCard' detailsStyles