module Main exposing (..)

import Calcul exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing(..)

main = Browser.sandbox { init = init, update = update, view = view }




type alias Model =
  { int1 : Maybe Float
  , int2 : Maybe Float

  }

init : Model
init =
  Model Nothing Nothing

--type Ope = C String  



-- UPDATE


type Msg
  = I1 (Result String Float)
  | I2 (Result String Float)
 


update : Msg -> Model -> Model
update msg {int1, int2 } =
  case msg of
    I1 (Ok int0) ->
      Model (Just int0)  int2 

    I1 (Err err ) -> 
        Model Nothing int2 

    I2 (Ok int0) ->
      Model int1 (Just int0) 

    I2 (Err err) ->
      Model int1 Nothing 



fabselect : List String -> List(Html msg)
fabselect l = List.map(\valeur -> option [value valeur] [ text valeur ]) l

toFloat2 : String -> Result String Float 
toFloat2 str = 
    case (String.toFloat str) of 
        Nothing -> Err "err"
        Just nb -> Ok nb

view : Model -> Html Msg
view model =     div [ ][
    text (affExpr expExpr2)
    ,br[][]
    ,input [ type_ "String", placeholder " Valeur de X  ", onInput ( I1<< toFloat2) ] []
    , input [ type_ "String", placeholder " Valeur de Y ", onInput (I2<<toFloat2) ] []
    ,br[][]
    ,br[][]
    ,let
        int1 : Maybe Float
        int1 = model.int1
        int2 : Maybe Float
        int2 = model.int2 
    in
        case (int1,int2) of 
            (Just x ,Just y) -> text (String.fromFloat (calcul (evalFold [(X,x), (Y,y)] expExpr2 )))
            _             -> text (" ")
  
    ]

--(3 + x ) * (x - y)
expExpr1 : Expr Var
expExpr1 =  fois (plus (Const 3 ) (Inconnue X))  (moins (Inconnue X)  ( Inconnue Y))

expExpr2 : Expr Var
expExpr2  = plus (puiss (cosinus (Inconnue X )) (Const 2)) (puiss (sinus (Inconnue Y))(Const 2))


affExpr : Expr Var -> String 
affExpr val = 
        case val of 
                OpeBin   { bin , exprg , exprd }   -> 
                    let 
                        entoure ope =  "("++( (affExpr exprg) ++ ope ++  (affExpr exprd))++")" 
                    in
                        case bin of 
                            Plus   ->   entoure " + "
                            Moins  ->   entoure " - "
                            Fois   ->   entoure " * "
                            Div    ->   entoure " / "
                            Puiss  ->   entoure "^"               
                OpeTrigo  { trigo , expr}   ->
                    let
                        suivant ope =  "("++ ope ++  (affExpr expr) ++ ")"
                    in
                        case trigo of   
                            Cos      ->   suivant "cos" 
                            Sin      ->   suivant "sin"
                            Tan      ->   suivant "tan"
                Const     f   ->   String.fromFloat f   
                Inconnue  v    ->  Debug.toString v
                
                    
