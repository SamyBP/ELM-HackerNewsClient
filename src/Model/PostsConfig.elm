module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time
import List exposing (sortBy)


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" ->
            Just Score

        "Title" ->
            Just Title

        "Posted" ->
            Just Posted

        "None" ->
            Just None

        _ ->
            Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ChangePostsToFetch Int
    | ChangePostsToShow Int
    | ChangeSortBy SortBy
    | ChangeShowJobs
    | ChangeShowTextOnly


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    -- Debug.todo "applyChanges"
    case change of
        ChangePostsToFetch count -> 
            { config | postsToFetch = count }
        
        ChangePostsToShow count -> 
            { config | postsToShow = count }
        
        ChangeSortBy criteria -> 
             { config | sortBy = criteria }
        
        ChangeShowJobs -> 
            { config | showJobs = not config.showJobs }
        
        ChangeShowTextOnly -> 
            { config | showTextOnly = not config.showTextOnly }


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        filteredPosts =
            applyFilters config posts
        
        takenPosts =
            List.take config.postsToShow filteredPosts

        sortedPosts =
            applySorting config.sortBy takenPosts
    in
        sortedPosts

applyFilters : PostsConfig -> List Post -> List Post
applyFilters config posts =
    List.filter (\post ->
        (config.showJobs || post.type_ /= "job")
            &&
        (config.showTextOnly || Maybe.withDefault "" post.url /= "")
    ) posts

applySorting : SortBy -> List Post -> List Post
applySorting sortBy posts =
    let
        compareFunction = 
            computeCompareFunctionFromCriteria sortBy
    in 
        List.sortWith compareFunction posts

computeCompareFunctionFromCriteria : SortBy -> (Post -> Post -> Order)
computeCompareFunctionFromCriteria sortBy = 
    case sortBy of
        Title ->
            \postA postB ->
                compare
                    (String.toLower postA.title)
                    (String.toLower postB.title)

        Score ->
            \postA postB ->
                compare postB.score postA.score

        Posted ->
            \postA postB ->
                compare
                    (Time.posixToMillis postB.time)
                    (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ