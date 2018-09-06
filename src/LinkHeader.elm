module LinkHeader exposing
    ( WebLink, LinkRel(..)
    , parse
    )

{-| This library provides the ability to parse Link headers returned from APIs.


# Types

@docs WebLink, LinkRel


# Parsing

@docs parse

-}

import Parser exposing ((|.), (|=), Parser)
import Set
import Url
import Url.Parser
import Url.Parser.Query


{-| The possible types of web links that this parser might produce.
-}
type LinkRel
    = RelNext Int
    | RelPrev Int
    | RelFirst Int
    | RelLast Int
    | RelUnknown Int


{-| An individual link contains a `rel`, the `page` number and the URL.
-}
type alias WebLink =
    { rel : LinkRel
    , url : String
    }


resultIsError : Result x a -> Bool
resultIsError result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


{-| Parse is given the string, which should be the entire link header from your API and returns a list of WebLinks. This will be empty if no matches were found
-}
parse : String -> List WebLink
parse str =
    str
        |> String.split ","
        |> List.map String.trim
        |> List.map parseHeader
        |> List.filterMap Result.toMaybe


relStringToType : String -> Int -> LinkRel
relStringToType string page =
    case string of
        "next" ->
            RelNext page

        "last" ->
            RelLast page

        "first" ->
            RelFirst page

        "prev" ->
            RelPrev page

        _ ->
            RelUnknown page


initialParser : Parser { url : String, rel : String }
initialParser =
    Parser.succeed (\u r -> { url = u, rel = r })
        |. Parser.symbol "<"
        |= Parser.variable
            { start = Char.isLower
            , inner = (/=) '>'
            , reserved = Set.empty
            }
        |. Parser.symbol ">"
        |. Parser.symbol ";"
        |. Parser.spaces
        |. Parser.keyword "rel="
        |. Parser.symbol "\""
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        |. Parser.symbol "\""
        |. Parser.end


linkParser : { url : String, rel : String } -> Parser WebLink
linkParser { url, rel } =
    let
        urlParser =
            Url.Parser.query (Url.Parser.Query.int "page")

        parsePage u =
            u
                |> (\url_ -> { url_ | path = "" })
                |> Url.Parser.parse urlParser
                |> Maybe.andThen identity

        maybePage : Maybe Int
        maybePage =
            url
                |> Url.fromString
                |> Maybe.andThen parsePage
    in
    case maybePage of
        Nothing ->
            Parser.problem "invalid URL"

        Just page ->
            Parser.succeed <| WebLink (relStringToType rel page) url


parseHeader : String -> Result (List Parser.DeadEnd) WebLink
parseHeader =
    Parser.run (initialParser |> Parser.andThen linkParser)
