module Main exposing (..)

import Browser
import Random
import Html exposing (Html, h1, h2, p, text, button, div, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import List.Extra as List
    

-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { deck : List Card
    , picked : List Card
    , activeCard : Maybe Card
    , startingPos : List Position
    , availableExplorerPos : List Int
    , availableTreasuryPos : List Int
    }

type alias Card =
    { number : Int
    , paths : Int
    , cardType : CardType
    , img : String
    }

initDeck : List Card
initDeck =
    [ Card 1 2 Plain ""
    , Card 2 2 Plain ""
    , Card 3 2 Crystal ""
    , Card 4 2 Crystal ""
    , Card 5 3 Plain ""
    , Card 6 3 Plain ""
    , Card 7 4 Plain ""
    , Card 8 4 Plain ""
    , Card 9 2 Crystal ""
    , Card 10 2 Crystal ""
    , Card 11 3 Crystal ""
    , Card 12 3 Plain ""
    , Card 13 2 Crystal ""
    , Card 14 2 Gold ""
    , Card 15 2 Crystal ""
    , Card 16 2 Gold ""
    , Card 17 2 Plain ""
    , Card 18 2 Plain ""
    , Card 19 2 Plain ""
    , Card 20 2 Plain ""
    , Card 21 2 Plain ""
    , Card 22 2 Plain ""
    , Card 23 3 Plain ""
    , Card 24 3 Plain ""
    , Card 25 4 Plain ""
    , Card 26 4 Plain ""
    , Card 27 2 Crystal ""
    , Card 28 2 Plain ""
    , Card 29 3 Crystal ""
    , Card 30 3 Plain ""
    , Card 31 2 Crystal ""
    , Card 32 2 Gold ""
    , Card 33 2 Crystal ""
    , Card 34 2 Gold ""
    , Card 35 2 Plain ""
    , Card 36 2 Plain ""
    ]


init : () -> (Model, Cmd Msg)
init _ =
  ( initModel, initPositions initModel )


initModel : Model
initModel =
    { deck = initDeck
    , picked = []
    , activeCard = Nothing
    , startingPos = []
    , availableExplorerPos = List.range 1 11
    , availableTreasuryPos = List.range 1 11
    }


bugCard : Card
bugCard =
    Card 0 0 Bug ""


type Msg
    = CardPicked Int
    | RequestCard
    | Restart
    | ExplorerPosPicked (Color, Int)
    | TreasuryPosPicked (Color, Int)


type CardType
    = Plain
    | Crystal
    | Gold
    | Bug 


type alias Position =
    { color : Color
    , explorer : Int
    , treasury : Int
    } 


type Color
    = Brown
    | Purple
    | Blue
    | Yellow


update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        CardPicked cardPosition ->
            ( spendCard model cardPosition, Cmd.none )

        RequestCard ->
            ( model, randomCard model.deck)

        Restart ->
            ( initModel, initPositions model )

        ExplorerPosPicked (color, pos) ->
            ( updateExplorerPos model color pos, pickTreasuryPos model color )

        TreasuryPosPicked (color, pos) ->
            ( updateTreasuryPos model color pos, Cmd.none )


initPositions : Model -> Cmd Msg
initPositions model =
    Cmd.batch [pickExplorerPos model Brown, pickExplorerPos model Purple, pickExplorerPos model Blue, pickExplorerPos model Yellow]


updatePosition : Position -> Color -> Int -> Int -> Position
updatePosition position color pos1 pos2 =
    if position.color == color then
        Position color pos1 pos2
    else
        position

updateExplorerPos : Model -> Color -> Int -> Model
updateExplorerPos model color pos =
    let 
        updatedPos = Position color pos 0 :: model.startingPos
        newAvailableExplorer =  List.filter ((/=) pos) model.availableExplorerPos

    in
        { model | startingPos = updatedPos, availableExplorerPos = newAvailableExplorer }


pickExplorerPos : Model -> Color -> Cmd Msg
pickExplorerPos model color =
    Random.generate (\x -> ExplorerPosPicked (color, x)) (Random.int 1 (List.length model.availableExplorerPos))

    
updateTreasuryPos : Model -> Color -> Int -> Model
updateTreasuryPos model color pos =
    let 
        updatedPos = List.map (\x -> updatePosition x color x.explorer pos) model.startingPos
        newAvailableTreasury =  List.filter ((/=) pos) model.availableTreasuryPos

    in
        { model | startingPos = updatedPos, availableTreasuryPos = newAvailableTreasury}
        

pickTreasuryPos : Model -> Color -> Cmd Msg
pickTreasuryPos model color =
    Random.generate (\x -> TreasuryPosPicked (color, x)) (Random.int 1 (List.length model.availableTreasuryPos))



spendCard : Model -> Int -> Model
spendCard model cardPosition =
    let
        pickedCard = pickFromRandom model.deck cardPosition
        remainingDeck = List.filter (cardMatch pickedCard) model.deck
        pickedDeck = Maybe.withDefault bugCard pickedCard :: model.picked
    in
        { model | deck = remainingDeck, picked = pickedDeck, activeCard = pickedCard }


pickFromRandom : List a -> Int -> Maybe a
pickFromRandom list position =
    if position == 1 then
        List.head list
    else
        pickFromRandom (List.drop (position - 1) list) 1


cardMatch :  Maybe Card -> Card -> Bool
cardMatch pickedCard givenCard =
    ( Maybe.withDefault bugCard pickedCard).number /= givenCard.number


randomCard : List Card -> Cmd Msg
randomCard deck =
    Random.generate CardPicked (Random.int 1 (List.length deck))
    

startingPosition : List Position
startingPosition =
    List.foldr generatePosition [] [Brown, Purple, Blue, Yellow]


generatePosition : Color -> List Position -> List Position
generatePosition color positionsCreated = 
    let
        availableExplorePosition = List.filter (\x -> List.notMember x (List.map (\ep -> ep.explorer ) positionsCreated) ) (List.range 1 11) 
        availableTreasuryPosition = List.filter (\x -> List.notMember x (List.map (\tp -> tp.treasury ) positionsCreated) ) (List.range 1 11)
    in
        Position color 1 1 :: positionsCreated


-- VIEW


view : Model -> Html Msg
view model =
    Grid.container [] (CDN.stylesheet ::
    case model.activeCard of
        Maybe.Nothing ->
            displayStart model

        Just card ->
            displayInGame model card
    )

displayStart : Model -> List (Html Msg)
displayStart model =
    [ Grid.row
        [ Row.topXs ]
        [ Grid.col
            [ Col.xs12 ]
            [ h1 [] [ text "Karuba Card Picker" ] ]
        ]
    , Grid.row
        [ Row.topXs ]
        [ Grid.col
            [ Col.xs12 ]
            [ Button.button [Button.primary, Button.attrs [onClick RequestCard]] [text "First Card"] ]
        ]
    ]
    -- div [class "container"] 
    --     [ h1 [] [text "Karuba Card Picker"]
    --     , Button.button [Button.primary, Button.attrs [onClick RequestCard]] [text "First Card"]
    --     , ul [] (listStartingPosition model.startingPos)
    --     ,  Button.button [Button.warning, Button.attrs [onClick Restart]] [text "Restart"]

    --     ]

displayInGame : Model -> Card -> List (Html Msg)
displayInGame model card =
    [case List.tail model.deck of

        Just _ -> 
            div [class "container"] 
                [ h1 [] [text "Karuba Card Picker"]
                , Button.button [Button.primary, Button.attrs [onClick RequestCard]] [text "Next Card"]
                , div []
                    [ h2 [] [ text (String.concat ["Card Number: ", String.fromInt card.number])]
                    , p [] [ text (String.concat ["Paths: ", String.fromInt card.paths])] ]
                    , h2 [] [ text "Remaining cards"]
                    , p [] [ text (String.fromInt (List.length model.deck))]
                    , ul [] (listStartingPosition model.startingPos)
                    , h2 [] [ text "Picked cards"]
                    , p [] [ text  (String.concat (List.intersperse ", " (List.map (\c -> String.fromInt c.number) model.picked) ) ) ]
                    ,  Button.button [Button.danger, Button.attrs [onClick Restart]] [text "Restart"]
  
                ] 
        
        Nothing ->
            div [class "container"] 
                [ h1 [] [text "Karuba Card Picker"]
                , p [] [Alert.simpleWarning [] [text "Out of cards"]]
                , Button.button [Button.warning, Button.attrs [onClick Restart]] [text "Restart"]
                , div []
                    [ h2 [] [ text (String.concat ["Card Number: ", String.fromInt card.number])]
                    , p [] [ text (String.concat ["Paths: ", String.fromInt card.paths])] ]
                    , ul [] (listStartingPosition model.startingPos)
                    , h2 [] [ text "Picked cards"]
                    , p [] [ text  (String.concat (List.intersperse ", " (List.map (\c -> String.fromInt c.number) model.picked) ) ) ]  
                ] 
    ]

listStartingPosition : List Position -> List (Html Msg)
listStartingPosition startingPos =
    List.map (\x -> ul [] [ text (String.concat [colorToString x.color, " Explorador: ", String.fromInt (x.explorer * 10), ", Treasury: ", String.fromInt x.treasury, "0."] ) ] ) startingPos


colorToString : Color -> String
colorToString color =
    case color of
       Blue -> "Blue"
       Brown -> "Brown"
       Yellow -> "Yellow"
       Purple -> "Purple"

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none