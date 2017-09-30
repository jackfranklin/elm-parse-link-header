module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import LinkHeader exposing (LinkRel(..))


nextUrl : String
nextUrl =
    "https://api.github.com/user/193238/repos?per_page=100&page=2"


lastUrl : String
lastUrl =
    "https://api.github.com/user/193238/repos?per_page=100&page=3"


header : String
header =
    Debug.log "header" ("<" ++ nextUrl ++ ">; rel=\"next\", <" ++ lastUrl ++ ">; rel=\"last\"")


suite : Test
suite =
    describe "GithubApi.parseLinkHeader"
        [ test "If there are no matches it returns an empty list" <|
            \() ->
                Expect.equal (LinkHeader.parse "") []
        , test "When given a header it can parse it" <|
            \() ->
                Expect.equal (LinkHeader.parse header)
                    [ (LinkHeader.WebLink (RelNext 2) nextUrl)
                    , (LinkHeader.WebLink (RelLast 3) lastUrl)
                    ]
        ]
