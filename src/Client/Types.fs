module Types
open Shared

type Page =
    | Search
    | Cocktail of int

let toHash = function
    | Search -> "#/search"
    | Cocktail id -> sprintf "#/cocktail/%d" id

type Model =
    { currentPage : Page
      keyword : string
      cocktails : Cocktail list
      cocktailDetails : CocktailDetails option
      isLoading : bool }

type Msg =
    | KeywordChanged of string
    | Search
    | SearchSuccess of Cocktail list
    | LoadCocktailDetails of int
    | CocktailDetailsSuccess of CocktailDetails option
    | Failure of exn
    | Load of bool