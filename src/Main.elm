module Main exposing (..)

import View as V
import View.Attributes as VA

import Browser
import Html exposing (..)
import String exposing (fromInt)
import List exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List.Extra exposing (..)


type alias Model = 
    {
        info : String
        , clickedidx : Maybe Int
        , elemHigh : Maybe Int 
    }

array : List (Int)
array = [43, 12, 325, 55, 23]

init: () -> (Model, Cmd V.Msg)
init _ = (
            {
                info = "Please click on any value/index"
                , clickedidx = Nothing
                , elemHigh = Nothing 
            }    
                , Cmd.none)

update: V.Msg -> Model -> (Model, Cmd V.Msg)
update msg model =
    case msg of 
        V.ElemSelect (val, idx) ->
            if (model.elemHigh /= Nothing && model.elemHigh /= Just idx) then (
                                                    {
                                                        model | info = "Selected value " ++ fromInt val ++ " at index " ++ fromInt idx
                                                        , clickedidx = Just idx
                                                        , elemHigh = Just idx
                                                    },Cmd.none)
            else (
                {
                    info = if model.elemHigh == Nothing then "Selected value " ++ fromInt val ++ " at index " ++ fromInt idx
                        else "Deselected value " ++ fromInt val ++ " at index " ++ fromInt idx
                    , clickedidx = Just idx
                    , elemHigh = case model.elemHigh of
                                    Nothing -> Just idx
                                    Just _ -> Nothing   
                }
                , Cmd.none)

view: Model -> Html V.Msg
view model = 
    Html.div[]
    [
        V.viewArray 
            [
                VA.height 10
                , VA.width 5    
                , VA.padding 2
                , VA.shape (VA.Rbox 3 2)
                , VA.fill "teal"
            ] 
            array 
            model.info
            model.clickedidx
            model.elemHigh
    ]
    

main : Program() Model V.Msg
main = 
    Browser.element
        {
            init = init
            , view = view
            , update = update
            , subscriptions = \_ -> Sub.none
        }