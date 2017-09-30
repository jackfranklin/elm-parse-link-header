module LinkHeader
    exposing
        ( WebLink
        , LinkRel(..)
        , parse
        , getIntegerForRel
        , webLinkIsNext
        , webLinkIsPrev
        , webLinkIsFirst
        , webLinkIsLast
        )

{-| This library provides the ability to parse Link headers returned from APIs.

# Types
@docs WebLink, LinkRel

# Parsing
@docs parse

# Reading results

@docs webLinkIsNext, webLinkIsPrev, webLinkIsFirst, webLinkIsLast, getIntegerForRel
-}

import Regex


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


{-| Parse is given the string, which should be the entire `link` header from your API
and returns a list of `WebLink`s. This will be empty if no matches were found.
    parse "<https://api.github.com/user/193238/repos?per_page=100&page=2>; rel=\"next\""  == [WebLink (RelNext 2) "https://api.github.com/user/193238/repos?per_page=100&page=2"]
-}
parse : String -> List WebLink
parse str =
    str
        |> String.split ","
        |> List.map runHeaderThroughRegex
        |> List.concat
        |> List.map regexMatchToWebLink
        |> List.filterMap identity


{-| Takes a `LinkRel` and finds the number for the page
    getIntegerForRel (RelPrev 2) == 2
-}
getIntegerForRel : LinkRel -> Int
getIntegerForRel linkRel =
    case linkRel of
        RelFirst x ->
            x

        RelLast x ->
            x

        RelNext x ->
            x

        RelPrev x ->
            x

        RelUnknown x ->
            x


{-| Tells you if a given `WebLink` is first.
-}
webLinkIsFirst : WebLink -> Bool
webLinkIsFirst link =
    case link.rel of
        RelFirst _ ->
            True

        _ ->
            False


{-| Tells you if a given `WebLink` is last.
-}
webLinkIsLast : WebLink -> Bool
webLinkIsLast link =
    case link.rel of
        RelLast _ ->
            True

        _ ->
            False


{-| Tells you if a given `WebLink` is the next link.
-}
webLinkIsNext : WebLink -> Bool
webLinkIsNext link =
    case link.rel of
        RelNext _ ->
            True

        _ ->
            False


{-| Tells you if a given `WebLink` is the prev link.
-}
webLinkIsPrev : WebLink -> Bool
webLinkIsPrev link =
    case link.rel of
        RelPrev _ ->
            True

        _ ->
            False


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


regexMatchToWebLink : Regex.Match -> Maybe WebLink
regexMatchToWebLink match =
    case ( match.submatches, getUrlFromLink match.match ) of
        ( [ Just pageNumber, Just rel ], Just url ) ->
            case (String.toInt pageNumber) of
                Ok pageNum ->
                    Just { rel = relStringToType rel pageNum, url = url }

                Err e ->
                    Nothing

        _ ->
            Nothing


runHeaderThroughRegex : String -> List Regex.Match
runHeaderThroughRegex =
    Regex.find (Regex.AtMost 1) (Regex.regex ".+&page=(\\d+).+rel=\"(\\w+)\"")


getUrlFromLink : String -> Maybe String
getUrlFromLink string =
    string
        |> Regex.find (Regex.AtMost 1) (Regex.regex "<(.+)>")
        |> List.map (.submatches >> List.filterMap identity)
        |> List.concat
        |> List.head
