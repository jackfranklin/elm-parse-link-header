module Tests exposing (header, lastUrl, nextUrl, suite)

import Expect exposing (Expectation)
import LinkHeader exposing (LinkRel(..))
import Test exposing (..)


nextUrl : String
nextUrl =
    "https://api.github.com/user/193238/repos?per_page=100&page=2"


lastUrl : String
lastUrl =
    "https://api.github.com/user/193238/repos?per_page=100&page=3"


shortUrl : String
shortUrl =
    "https://api.github.com/user/123/repos?page=4"


header : String
header =
    String.join ", "
        [ "<" ++ nextUrl ++ ">; rel=\"next\""
        , "<" ++ lastUrl ++ ">; rel=\"last\""
        , "<" ++ shortUrl ++ ">; rel=\"prev\""
        ]


suite : Test
suite =
    describe "GithubApi.parseLinkHeader"
        [ test "If there are no matches it returns an empty list" <|
            \() ->
                Expect.equal (LinkHeader.parse "") []
        , test "When given a header it can parse it" <|
            \() ->
                Expect.equal (LinkHeader.parse header)
                    [ LinkHeader.WebLink (RelNext 2) nextUrl
                    , LinkHeader.WebLink (RelLast 3) lastUrl
                    , LinkHeader.WebLink (RelPrev 4) shortUrl
                    ]
        ]
