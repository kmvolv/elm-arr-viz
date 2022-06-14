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
        , idx : VA.Index
    }

defArrConfig : ArrConfig
defArrConfig = 
    {
        height = 10
        , width = 10
        , padding = 10
        , shape = VA.Box
        , fill = "white"
        , idx = VA.Bottom
    }

type Msg 
    = ElemSelect (Int, Int, Bool)
    | SideIdx Int

getAtpos: List(Int) -> Int -> Int
getAtpos array pos = 
    Maybe.withDefault 0 (getAt pos array)

btnStyles : Int -> Int -> Int -> String -> List(Attribute Msg)
btnStyles checker idx hlightchecker fill = 
            [
                SA.fillOpacity "0.2"
                , SA.cursor "pointer"
                , if (checker == idx && hlightchecker == idx) then SA.style ("stroke-width:0.2;stroke:black;fill:lime; transition: fill 0.5s ease-in-out") 
                else SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
            ]

elemStyle : Int -> String -> Int-> List(Attribute Msg)
elemStyle idx fill hlightchecker = 
    [
        SA.fillOpacity "0.2"
        , SA.cursor "pointer"
        , if (hlightchecker == -1 || hlightchecker/=idx) then SA.style ("stroke-width:0.2;stroke:black;fill:" ++ fill ++ "; transition: fill 0.25s ease-in-out")
        else SA.style ("stroke-width:0.2;stroke:black;fill:lime; transition: fill 0.5s ease-in-out") 
    ]

idxStyle : Int -> String -> Int -> List(Attribute Msg)
idxStyle idx fill checker = 
        [
            SA.opacity "0"
            , SA.fillOpacity "0.2"
            , SA.cursor "pointer"
            , if (checker == -1 || checker/=idx) then SA.style ("stroke-width:0.02;stroke:black;opacity:0;fill:" ++ fill ++ "; transition: all 0.25s ease-in-out")
            else SA.style ("stroke-width:0.02;stroke:black;fill:lime;opacity:1; transition: all 0.5s ease-in-out") 
        ]

drawer: List(Int) -> Float -> Float -> Int -> VA.Shape -> String -> Int -> Maybe Int -> Maybe Int -> VA.Index -> List(Svg Msg)
drawer array hght wdth padding shape fill idx cidx hlight idxpos =
    let
        space = (round (wdth) + padding)*idx
        transX = fromFloat (Basics.toFloat space + (wdth/2))
        idxtransX = fromFloat (Basics.toFloat space + (wdth/2) + if idxpos == VA.Side then -3.5 else 0)
        transY = fromFloat (hght/2 + if idxpos == VA.Top then 5 else 0)
        idxtransY = fromFloat <| if idxpos == VA.Top then 2 else (hght + 3)
        val = getAtpos array idx
        checker = case cidx of 
                    Nothing -> -1
                    Just x -> x
        hlightchecker = case hlight of 
                            Nothing -> -1
                            Just x -> x
    in
    if idx == List.length array then
        if idxpos == VA.Side then
        [
            Svg.g[ SE.onClick <| SideIdx idx ]
                [
                    Svg.rect
                            ([ SA.x <| fromFloat <| 0.5 + Basics.toFloat (space-padding)
                            , SA.y <| fromInt (if idxpos == VA.Top then 5 else 0)
                            , SA.height (String.fromFloat hght)
                            , SA.width "1"
                            ] ++ idxStyle idx fill checker)[]
                    , Svg.text_
                        [
                            SA.textAnchor "middle"
                            , SA.dominantBaseline "central"
                            , SA.transform ("translate(" ++ idxtransX ++ "," ++ idxtransY ++ ")")
                            , SA.fontSize "2px"
                            , SA.cursor "pointer"
                        ] [ Svg.text <| fromInt idx]   
                ]
        ]
        else []
    else 
        Svg.g[ SE.onClick <| if idxpos /= VA.Side then ElemSelect(val,idx,False) else ElemSelect(val,idx,True) ]
            [
                case shape of 
                    VA.Box ->
                        Svg.rect
                            ([ SA.x (fromInt space)
                            , SA.y <| fromInt (if idxpos == VA.Top then 5 else 0)
                            , SA.height (String.fromFloat hght)
                            , SA.width (fromFloat wdth)
                            , SA.rx "0"
                            , SA.ry "0"
                            ] ++ btnStyles checker idx hlightchecker fill)[]
                    VA.Circle r -> 
                        Svg.circle
                            ([ SA.cx (fromFloat (wdth/2 + Basics.toFloat space))
                            , SA.cy <| fromFloat (hght/2 + if idxpos == VA.Top then 5 else 0)
                            , SA.r <| fromInt r
                            ] ++ btnStyles checker idx hlightchecker fill)[]
                    VA.Ellipse rx ry ->
                        Svg.ellipse
                            ([ SA.cx (fromFloat (wdth/2 + Basics.toFloat space))
                            , SA.cy <| fromFloat (hght/2 + if idxpos == VA.Top then 5 else 0)
                            , SA.rx <| fromInt rx
                            , SA.ry <| fromInt ry 
                            ] ++ btnStyles checker idx hlightchecker fill)[]
                    VA.Rbox rx ry ->
                        Svg.rect
                            ([ SA.x (fromInt space)
                            , SA.y <| fromInt (if idxpos == VA.Top then 5 else 0)
                            , SA.height (String.fromFloat hght)
                            , SA.width (fromFloat wdth)
                            , SA.rx <| fromInt rx
                            , SA.ry <| fromInt ry
                            ] ++ if idxpos == VA.Side then elemStyle idx fill hlightchecker else btnStyles checker idx hlightchecker fill)[]

                , Svg.text_
                [ SA.textAnchor "middle"
                , SA.dominantBaseline "central"
                , SA.transform ("translate(" ++ transX ++ "," ++ transY ++ ")")
                , SA.fontSize "2px"
                , SA.cursor "pointer"
                ][ Svg.text <| fromInt <| val ]
            ]
                :: if idxpos /= VA.Side then 
                    [
                        Svg.g [SE.onClick <| ElemSelect (val,idx,False)]
                        [
                            Svg.circle
                                [ SA.cx (fromFloat (wdth/2 + Basics.toFloat space + if idxpos == VA.Side then -3.5 else 0))
                                , SA.cy <| fromFloat <| if idxpos == VA.Top then 2 else (hght + 3)
                                , SA.r <| fromFloat 1.5
                                , SA.strokeWidth (if (checker == idx && hlightchecker == idx) then "0.15" else "0")
                                , SA.style ("stroke:red;fill:none; transition: stroke-width 0.25s ease-in-out")
                                , SA.fillOpacity "0.2"
                                , SA.cursor "pointer"
                                ][]
                            , Svg.text_
                            [
                                SA.textAnchor "middle"
                                , SA.dominantBaseline "central"
                                , SA.transform ("translate(" ++ idxtransX ++ "," ++ idxtransY ++ ")")
                                , SA.fontSize "2px"
                                , SA.cursor "pointer"
                            ] [ Svg.text <| fromInt idx]
                        ]
                    ]
                    ++ drawer array hght wdth padding shape fill (idx+1) cidx hlight idxpos
                else
                    [
                        Svg.g[ SE.onClick <| SideIdx idx ]
                            [
                                Svg.rect
                                        ([ SA.x <| fromFloat <| 0.5 + Basics.toFloat (space-padding)
                                        , SA.y <| fromInt (if idxpos == VA.Top then 5 else 0)
                                        , SA.height (String.fromFloat hght)
                                        , SA.width "1"
                                        ]  ++ idxStyle idx fill checker)[]
                                , Svg.text_
                                    [
                                        SA.textAnchor "middle"
                                        , SA.dominantBaseline "central"
                                        , SA.transform ("translate(" ++ idxtransX ++ "," ++ idxtransY ++ ")")
                                        , SA.fontSize "2px"
                                        , SA.cursor "pointer"
                                    ] [ Svg.text <| fromInt idx]   
                            ]
                    ]    
                    ++ drawer array hght wdth padding shape fill (idx+1) cidx hlight idxpos

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
        Svg.g[](drawer array hght wdth pding config.shape config.fill 0 cidx hlight config.idx)
        , Svg.text_[
            SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.fontSize "2px"
            , SA.transform <| "translate(" ++ String.fromInt midwdth ++ "," ++ String.fromFloat (hght+7) ++ ")"
        ]
        [Svg.text model]
            
    ]
