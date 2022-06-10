module View exposing (..)
import View.Attributes as VA

import Html exposing (Html)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE
import List.Extra exposing (..)


type alias ArrConfig = 
    {
        height : Float
        , width : Float
        , padding : Int
        , shape : VA.Shape
        , fill : String
    }

defArrConfig : ArrConfig
defArrConfig = 
    {
        height = 10
        , width = 10
        , padding = 10
        , shape = VA.Box
        , fill = "white"
    }

type Msg 
    = ElemSelect (Int, Int)

getAtpos: List(Int) -> Int -> Int
getAtpos array pos = 
    Maybe.withDefault 0 (getAt pos array)

drawer: List(Int) -> Float -> Float -> Int -> VA.Shape -> String -> Int -> Maybe Int -> Maybe Int -> List(Svg Msg)
drawer array hght wdth padding shape fill idx cidx hlight =
    let
        space = (round (wdth) + padding)*idx
        transX = fromFloat (Basics.toFloat space + (wdth/2))
        transY = fromFloat (hght/2)
        idxtransY = fromFloat (hght + 5)
        val = getAtpos array idx
        checker = case cidx of 
                    Nothing -> -1
                    Just x -> x
        hlightchecker = case hlight of 
                            Nothing -> -1
                            Just x -> x
    in
    if idx == List.length array then []
    else 
        Svg.g[ SE.onClick <| ElemSelect(val,idx) ]
            [
                case shape of 
                    VA.Box ->
                        Svg.rect
                            [ SA.x (fromInt space)
                            , SA.y "0"
                            , SA.height (String.fromFloat hght)
                            , SA.width (fromFloat wdth)
                            , SA.rx "0"
                            , SA.ry "0"
                            , SA.fillOpacity "0.2"
                            , SA.cursor "pointer"
                            , if (checker == idx && hlightchecker == -1) then SA.style ("stroke-width:0.2;stroke:black;fill:red; transition: fill 0.5s ease-in-out")
                            else SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
                            ][]
                    VA.Circle r -> 
                        Svg.circle
                            [ SA.cx (fromFloat (wdth/2 + Basics.toFloat space))
                            , SA.cy <| fromFloat (hght/2)
                            , SA.r <| fromInt r
                            , SA.fillOpacity "0.2"
                            , SA.cursor "pointer"
                            , if (checker == idx && hlightchecker == idx) then SA.style ("stroke-width:0.2;stroke:black;fill:lime; transition: fill 2s ease-in-out")
                            else SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
                            ][]
                    VA.Ellipse rx ry ->
                        Svg.ellipse
                            [ SA.cx (fromFloat (wdth/2 + Basics.toFloat space))
                            , SA.cy <| fromFloat (hght/2)
                            , SA.rx <| fromInt rx
                            , SA.ry <| fromInt ry 
                            , SA.fillOpacity "0.2"
                            , SA.cursor "pointer"
                            , if (checker == idx && hlightchecker == idx) then SA.style ("stroke-width:0.2;stroke:black;fill:lime; transition: fill 0.5s ease-in-out")
                            else SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
                            ][]
                    VA.Rbox rx ry ->
                        Svg.rect
                            [ SA.x (fromInt space)
                            , SA.y "0"
                            , SA.height (String.fromFloat hght)
                            , SA.width (fromFloat wdth)
                            , SA.rx <| fromInt rx
                            , SA.ry <| fromInt ry
                            , SA.fillOpacity "0.2"
                            , SA.cursor "pointer"
                            , if (checker == idx && hlightchecker == idx) then SA.style ("stroke-width:0.2;stroke:black;fill:lime; transition: fill 0.5s ease-in-out")
                            else SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
                            ][]
                , Svg.text_
                [ SA.textAnchor "middle"
                , SA.dominantBaseline "central"
                , SA.transform ("translate(" ++ transX ++ "," ++ transY ++ ")")
                , SA.fontSize "2px"
                , SA.cursor "pointer"
                ][ Svg.text <| fromInt <| val ]
                , Svg.circle
                            [ SA.cx (fromFloat (wdth/2 + Basics.toFloat space))
                            , SA.cy <| fromFloat (hght + 5)
                            , SA.r <| fromFloat (if (checker == idx && hlightchecker == idx) then 1.5 else 0)
                            , SA.style ("stroke-width:0.15;stroke:red;fill:none; transition: all 0.25s ease-in-out")
                            , SA.fillOpacity "0.2"
                            , SA.cursor "pointer"
                            ][]
                , Svg.text_
                [
                    SA.textAnchor "middle"
                    , SA.dominantBaseline "central"
                    , SA.transform ("translate(" ++ transX ++ "," ++ idxtransY ++ ")")
                    , SA.fontSize "2px"
                    , SE.onClick <| ElemSelect(val,idx)
                    , SA.cursor "pointer"
                ] [ Svg.text <| fromInt idx]
            ]
        :: drawer array hght wdth padding shape fill (idx+1) cidx hlight

viewArray: List (VA.Attribute ArrConfig) -> List(Int) -> String -> Maybe Int -> Maybe Int -> Html Msg
viewArray mods array model cidx hlight = 
    let
        config = List.foldl (\f a -> f a) defArrConfig mods
        hght = config.height
        wdth = config.width
        pding = config.padding
        midwdth = ((List.length array)*(pding + round wdth))//2
        cWdth = (Basics.toFloat <| List.length array)*(wdth+ Basics.toFloat pding) + 5
        cHght = hght + 20
    in
    Svg.svg [
        SA.viewBox ("-5 -5 " ++ String.fromFloat (cWdth*1.2) ++ " " ++ String.fromFloat (cHght*1.2))
        , SA.height "80vh" , SA.width "100%"
    ]
    [
        Svg.g[](drawer array hght wdth pding config.shape config.fill 0 cidx hlight)
        , Svg.text_[
            SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.fontSize "2px"
            , SA.transform <| "translate(" ++ String.fromInt midwdth ++ "," ++ String.fromFloat (hght+10) ++ ")"
        ]
        [Svg.text model]
            
    ]
