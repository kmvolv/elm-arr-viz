module View.Attributes exposing (..)

type alias Attribute c = 
    c -> c

type Shape = 
    Box
    | Circle Int
    | Ellipse Int Int 
    | Rbox Int Int

type Index = 
    Top
    | Bottom
    | Side

height: Float -> Attribute { c | height : Float }
height f = 
    \cc ->
        { cc | height = f }

width: Float -> Attribute { c | width : Float }
width f = 
    \cc ->
        { cc | width = f }

padding: Int -> Attribute { c | padding : Int }
padding f = 
    \cc ->
        { cc | padding = f }

shape: Shape -> Attribute { c | shape : Shape }
shape f = 
    \cc ->
        { cc | shape = f}

fill: String -> Attribute { c | fill : String }
fill f = 
    \cc ->
        { cc | fill = f}

idx : Index -> Attribute { c | idx : Index}
idx f = 
    \cc ->
        { cc | idx = f }