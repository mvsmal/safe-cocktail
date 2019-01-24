module Types

type Model = {
    keyword : string
}

type Msg =
    | KeywordChanged of string
    | Search