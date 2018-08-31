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
        |> List.map toWebLink


toWebLink : HeaderMatch -> WebLink
toWebLink headerMatch =
    { url = headerMatch.url
    , rel = relStringToType headerMatch.rel headerMatch.page
    }


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


type alias HeaderMatch =
    { page : Int
    , rel : String
    , url : String
    }


headerParser : Parser HeaderMatch
headerParser =
    Parser.succeed
        (\urlStart page urlEnd source rel ->
            { page = page
            , url = String.slice urlStart urlEnd source
            , rel = rel
            }
        )
        |. Parser.symbol "<"
        |= Parser.getOffset
        |. Parser.chompUntil "&page="
        |. Parser.token "&page="
        |= Parser.int
        |= Parser.getOffset
        |= Parser.getSource
        |. Parser.chompUntil ">"
        |. Parser.symbol ">"
        |. Parser.chompUntil "rel="
        |. Parser.token "rel="
        |. Parser.symbol "\""
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isAlpha
            , reserved = Set.fromList []
            }


parseHeader : String -> Result (List Parser.DeadEnd) HeaderMatch
parseHeader header =
    Parser.run headerParser header
