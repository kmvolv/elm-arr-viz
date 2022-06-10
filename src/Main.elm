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
    String

array : List (Int)
array = [43, 12, 325, 55, 23]

init: () -> (Model, Cmd V.Msg)
init _ = ("Please click on any element", Cmd.none)

update: V.Msg -> Model -> (Model, Cmd V.Msg)
update msg _ =
    case msg of 
        V.ElemSelect (val, idx) ->
            ("Selected element " ++ fromInt val ++ " at index " ++ fromInt idx, Cmd.none)

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
            model
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