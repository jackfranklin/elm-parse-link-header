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


removeTokens : String -> String
removeTokens str =
    str
        |> String.replace "<" ""
        |> String.replace ">" ""


headerParserForUrl : Parser String
headerParserForUrl =
    (Parser.getChompedString <|
        (Parser.succeed ()
            |. Parser.symbol "<"
            |. Parser.chompUntil ">"
            |. Parser.symbol ">"
        )
    )
        |> Parser.andThen (Parser.succeed << removeTokens)


parsePageAndRel : Parser { page : Int, rel : String }
parsePageAndRel =
    Parser.succeed (\p r -> { page = p, rel = r })
        |. Parser.chompUntil "&page="
        |. Parser.token "&page="
        |= Parser.int
        |. Parser.chompUntil "rel="
        |. Parser.token "rel="
        |. Parser.symbol "\""
        |= Parser.variable { start = Char.isLower, inner = Char.isAlpha, reserved = Set.fromList [] }


parseDetails : String -> String -> Parser HeaderMatch
parseDetails originalInputString details =
    case Parser.run parsePageAndRel originalInputString of
        Err e ->
            Parser.problem "Problem parsing details"

        Ok { page, rel } ->
            Parser.succeed { page = page, rel = rel, url = details }


headerParser : String -> Parser HeaderMatch
headerParser originalInputString =
    headerParserForUrl |> Parser.andThen (parseDetails originalInputString)


parseHeader : String -> Result (List Parser.DeadEnd) HeaderMatch
parseHeader header =
    Parser.run (headerParser header) header
