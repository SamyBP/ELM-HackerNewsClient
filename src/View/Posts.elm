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
        , Html.td [ Html.Attributes.class "post-time" ] [ Html.text (Util.Time.formatTime Time.utc post.time) ]
        , Html.td [ Html.Attributes.class "post-url" ]
            [ Html.a [ Html.Attributes.href (Maybe.withDefault "#" post.url) ] [ Html.text "Link" ] ]
        ]

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
        [ Html.div []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (\_ -> ConfigChanged ChangeShowJobs)
                ]
                []
            , Html.label [ Html.Attributes.for "checkbox-show-job-posts" ] [ text "Show job posts" ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (ConfigChanged << (\_ -> ChangeShowTextOnly))
                ]
                []
            , Html.label [ Html.Attributes.for "checkbox-show-text-only-posts" ] [ text "Show text-only posts" ]
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "select-posts-per-page" ] [ text "Posts per page: " ]
            , Html.select
                [ Html.Attributes.id "select-posts-per-page"
                , Html.Events.onInput ((String.toInt >> Maybe.withDefault config.postsToShow) >> (ConfigChanged << ChangePostsToShow))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (String.fromInt option)
                            , if option == config.postsToShow then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (String.fromInt option) ]
                    )
                    [10, 25, 50]
                )
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "select-sort-by" ] [ text "Sort by: " ]
            , Html.select
                [ Html.Attributes.id "select-sort-by"
                , Html.Events.onInput ((Config.sortFromString >> Maybe.withDefault config.sortBy) >> (ConfigChanged << ChangeSortBy))
                ]
                (List.map
                    (\option ->
                        Html.option
                            [ Html.Attributes.value (Config.sortToString option)
                            , if option == config.sortBy then Html.Attributes.selected True else Html.Attributes.selected False
                            ]
                            [ text (Config.sortToString option) ]
                    )
                    Model.PostsConfig.sortOptions
                )
            ]
        ]
