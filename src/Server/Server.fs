open System.IO
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open FSharp.Data

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

[<Literal>]
let searchSampleUrl = "https://www.thecocktaildb.com/api/json/v1/1/search.php?s=margarita"
[<Literal>]
let searchUrl = "https://www.thecocktaildb.com/api/json/v1/1/search.php?s="

type CocktailSearchProvider = JsonProvider<searchSampleUrl>

let mapSearchResults (drink : CocktailSearchProvider.Drink) =
    { Id = drink.IdDrink
      Name = drink.StrDrink
      Image = drink.StrDrinkThumb }

let searchCocktails keyword =
    async {
        try
            let! result = CocktailSearchProvider.AsyncLoad(searchUrl + keyword)
            return result.Drinks |> Array.map mapSearchResults |> List.ofArray |> Ok
        with
        | _ -> return Error "Something went wrong"
    }

[<Literal>]
let detailsSampleUrl = "api-data/details.json"
[<Literal>]
let detailsUrl = "https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i="

type CocktailDetailsProvider = JsonProvider<detailsSampleUrl>

let mapIngredient name measure =
    { Name = name
      Measure = measure
      Image = sprintf "https://www.thecocktaildb.com/images/ingredients/%s-Small.png" name }

let mapIngredients (drink : CocktailDetailsProvider.Drink) =
    [
        if drink.StrIngredient1 <> "" then
            yield mapIngredient drink.StrIngredient1 drink.StrMeasure1
        if drink.StrIngredient2 <> "" then
            yield mapIngredient drink.StrIngredient2 drink.StrMeasure2
        if drink.StrIngredient3 <> "" then
            yield mapIngredient drink.StrIngredient3 drink.StrMeasure3
        if drink.StrIngredient4 <> "" then
            yield mapIngredient drink.StrIngredient4 drink.StrMeasure4
        if drink.StrIngredient5 <> "" then
            yield mapIngredient drink.StrIngredient5 drink.StrMeasure5
        if drink.StrIngredient6 <> "" then
            yield mapIngredient drink.StrIngredient6 drink.StrMeasure6
        if drink.StrIngredient7 <> "" then
            yield mapIngredient drink.StrIngredient7 drink.StrMeasure7
        if drink.StrIngredient8 <> "" then
            yield mapIngredient drink.StrIngredient8 drink.StrMeasure8
        if drink.StrIngredient9 <> "" then
            yield mapIngredient drink.StrIngredient9 drink.StrMeasure9
        if drink.StrIngredient10 <> "" then
            yield mapIngredient drink.StrIngredient10 drink.StrMeasure10
        if drink.StrIngredient11 <> "" then
            yield mapIngredient drink.StrIngredient11 drink.StrMeasure11
        if drink.StrIngredient12 <> "" then
            yield mapIngredient drink.StrIngredient12 drink.StrMeasure12
        if drink.StrIngredient13 <> "" then
            yield mapIngredient drink.StrIngredient13 drink.StrMeasure13
        if drink.StrIngredient14 <> "" then
            yield mapIngredient drink.StrIngredient14 drink.StrMeasure14
        if drink.StrIngredient15 <> "" then
            yield mapIngredient drink.StrIngredient15 drink.StrMeasure15
    ]


let mapDetails (drink : CocktailDetailsProvider.Drink) =
    { Id = drink.IdDrink
      Name = drink.StrDrink
      Image = drink.StrDrinkThumb
      Category = drink.StrCategory
      Glass = drink.StrGlass
      Alchogolic = drink.StrAlcoholic
      Instructions = drink.StrInstructions
      Ingredients = mapIngredients drink }
let cocktailDetails id =
    async {
        try
            let! result = CocktailDetailsProvider.AsyncLoad(detailsUrl + (string id))
            match result.Drinks with
            | [| drink |] -> return drink |> mapDetails |> Some |> Ok
            | _           -> return None  |> Ok
        with
        | _ -> return Error "Something went wrong"
    }

let cocktailApi = {
    search = searchCocktails
    details = cocktailDetails
}

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue cocktailApi
    |> Remoting.buildHttpHandler

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_gzip
}

run app
