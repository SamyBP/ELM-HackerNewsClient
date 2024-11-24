module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time
import Model.PostsConfig as Config


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config now posts =
    let
        filteredPosts = 
          filterPosts config posts    
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Score" ]
                , Html.th [] [ Html.text "Title" ]
                , Html.th [] [ Html.text "Type" ]
                , Html.th [] [ Html.text "Posted date" ]
                , Html.th [] [ Html.text "Link" ]
                ]
            ]
        , Html.tbody []
            (List.map (postTableRow now) filteredPosts)
        ]
    

postTableRow : Time.Posix -> Post -> Html Msg
postTableRow now post = 
  Html.tr []
        [ Html.td [ Html.Attributes.class "post-score" ] [ Html.text (String.fromInt post.score) ]
        , Html.td [ Html.Attributes.class "post-title" ] [ Html.text post.title ]
        , Html.td [ Html.Attributes.class "post-type" ] [ Html.text post.type_ ]
        , Html.td [ Html.Attributes.class "post-time" ] [ Html.text (formatPostTime now post.time) ]
        , Html.td [ Html.Attributes.class "post-url" ]
            [ Html.a [ Html.Attributes.href (Maybe.withDefault "#" post.url) ] [ Html.text "Follow this link to find out more" ] ]
        ]

formatPostTime : Time.Posix -> Time.Posix -> String
formatPostTime now postTime =
    let
        formattedDate = Util.Time.formatDate (Util.Time.posixToDate Time.utc postTime)
        formattedTime = Util.Time.formatTime Time.utc postTime
        relativeTime =
            case Util.Time.durationBetween postTime now of
                Just duration ->
                    Util.Time.formatDuration duration

                Nothing ->
                    "Just now"
    in
    formattedDate ++ " " ++ formattedTime ++ " (" ++ relativeTime ++ ")"




{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}

postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ checkbox
            "checkbox-show-job-posts"
            "Show job posts"
            config.showJobs
            (\_ -> ConfigChanged ChangeShowJobs)
        , checkbox
            "checkbox-show-text-only-posts"
            "Show text-only posts"
            config.showTextOnly
            (\_ -> ConfigChanged ChangeShowTextOnly)
        , dropdown
            "select-posts-per-page"
            "Posts per page: "
            (List.map String.fromInt [10, 25, 50])
            (String.fromInt config.postsToShow)
            (\selectedValue -> ConfigChanged (ChangePostsToShow (String.toInt selectedValue |> Maybe.withDefault config.postsToShow)))
        , dropdown
            "select-sort-by"
            "Sort by: "
            (List.map Config.sortToString Model.PostsConfig.sortOptions)
            (Config.sortToString config.sortBy)
            (\selectedValue -> ConfigChanged (ChangeSortBy (Config.sortFromString selectedValue |> Maybe.withDefault config.sortBy)))
        ]

checkbox : String -> String -> Bool -> (Bool -> Msg) -> Html Msg
checkbox id labelText isChecked onCheck =
    Html.div []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.id id
            , Html.Attributes.checked isChecked
            , Html.Events.onCheck onCheck
            ]
            []
        , Html.label [ Html.Attributes.for id ] [ text labelText ]
        ]

dropdown : String -> String -> List String -> String -> (String -> Msg) -> Html Msg
dropdown id labelText options selectedValue onChange =
    Html.div []
        [ Html.label [ Html.Attributes.for id ] [ text labelText ]
        , Html.select
            [ Html.Attributes.id id
            , Html.Events.onInput onChange
            ]
            (List.map (renderOption selectedValue) options)
        ]

renderOption : String -> String -> Html Msg
renderOption selectedValue option =
    Html.option
        [ Html.Attributes.value option
        , Html.Attributes.selected (option == selectedValue)
        ]
        [ text option ]
