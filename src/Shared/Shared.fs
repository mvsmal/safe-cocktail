namespace Shared

type Cocktail =
    { Id : int
      Name : string
      Image : string }

type Ingredient =
    { Name : string
      Measure : string
      Image : string }

type CocktailDetails =
    { Id : int
      Name : string
      Image : string
      Category : string
      Alchogolic : string
      Glass : string
      Instructions : string
      Ingredients : Ingredient list }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type CocktailApi =
    { search : string -> Async<Result<Cocktail list, string>>
      details : int -> Async<Result<CocktailDetails option, string>> }
