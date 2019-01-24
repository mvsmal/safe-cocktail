module Components
open Fable.MaterialUI.Core
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Fable.MaterialUI.Themes
open Fable.MaterialUI.Core
open Fable.MaterialUI.Props
open Fable.Import.React
open Types

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


let private appBarStyles (_ : ITheme) : IStyles list =
    [ Styles.Root [ FlexGrow 1 ] ]

let private myAppBar' _ _ classes =
    div [ Class !!classes?root ] [
        appBar [ AppBarProp.Position AppBarPosition.Static ] [
            toolbar [] [
                typography [
                    TypographyProp.Variant TypographyVariant.H6
                    MaterialProp.Color ComponentColor.Secondary
                ] [ str "SAFE Cocktail Search" ]
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