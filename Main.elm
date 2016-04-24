import Html as H
import String exposing (join)
import Html.Attributes as HA
import Svg 
import List.Extra as LE exposing (andThen)
import Signal 
import Svg.Events exposing (onClick, onMouseOver)
import Svg.Attributes exposing (version, viewBox, width, height, fill, stroke, strokeWidth, style, points, transform)

w = 1000
h = 600

rowCount = 20
colCount = 30

type alias Cell = (Int, Int)

type alias Model = 
    { rows : Int
    , cols : Int
    , selected : Maybe Cell
    , visited : Maybe Cell
    }

init rc cc = 
    { rows=rc
    , cols=cc
    , selected = Nothing
    , visited = Nothing
    }

center = HA.style [ ( "text-align", "center") ] 

view address model = 
    let
        radius = 0.5
        halfRadius = radius / 2.0
        halfWidth = radius * sqrt(3.0) / 2.0

        radiusStr = toString radius
        halfRadiusStr = toString halfRadius
        halfWidthStr = toString halfWidth

        shift row = if row % 2 == 0 then 0 else 0.5

        showCell row col = 
            Svg.polygon [ points <| 
                                " 0," ++ radiusStr
                             ++ "   " ++ halfWidthStr ++ "," ++ halfRadiusStr
                             ++ "   " ++ halfWidthStr ++ ",-" ++ halfRadiusStr
                             ++ " 0,-" ++ radiusStr
                             ++ "  -" ++ halfWidthStr ++ ",-" ++ halfRadiusStr
                             ++ "  -" ++ halfWidthStr ++ "," ++ halfRadiusStr
                        , transform <| "translate(" ++ toString (toFloat col + shift row) ++ " " ++ toString row  ++ ")"
                        , fill <| let jrc = Just (row, col) 
                                  in if jrc == model.selected 
                                     then "red" 
                                     else if jrc == model.visited 
                                          then "blue" 
                                          else "grey"
                        , stroke <| "black" 
                        , strokeWidth <| "0.07"
                        , onClick <| Signal.message address <| SetSelected (row, col)
                        , onMouseOver <| Signal.message address <| SetVisited (row, col)
                        ]
                        [] 

        cells model = [0..model.rows-1] `LE.andThen` \r ->
                      [0..model.cols-1] `LE.andThen` \c ->
                      [showCell r c]

    in 
        H.div 
          []
          [ H.h2 [center] [H.text <| toString model.selected]
          , H.h2 [center] [H.text <| toString model.visited]
          , H.div 
              [center] 
              [ Svg.svg 
                  [ version "1.1"
                  , width (toString w)
                  , height (toString h)
                  , viewBox (join " " 
                               [ -1.0                     |> toString
                               , -1.0                     |> toString
                               , toFloat model.cols + 1.0 |> toString
                               , toFloat model.rows + 1.0 |> toString ])
                  ] 
                  [ Svg.g [] <| cells model ]
              ]
          ] 

update action model =
    case action of
        SetSelected cell -> 
            {model |  selected = Just cell} 
        SetVisited cell -> 
            {model |  visited = Just cell} 
        NoOp -> model

control = Signal.mailbox NoOp

type Action = NoOp | SetSelected Cell | SetVisited Cell

modelSignal = Signal.foldp update (init rowCount colCount) control.signal

main = Signal.map (view control.address) modelSignal 
