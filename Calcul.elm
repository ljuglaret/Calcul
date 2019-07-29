module Calcul exposing(..)

type Bin = Plus 
             | Moins 
             | Fois 
             | Div      
             | Puiss    
 

type Trigo =  Cos       
            | Sin      
            | Tan      



type Expr a = OpeBin   { bin  : Bin , exprg : Expr a, exprd : Expr a}
            | Const     Float
            | Inconnue  a
            | OpeTrigo  { trigo : Trigo, expr : Expr a} 

type Var = X  
            | Y  
            | Z


plus :  Expr a ->  Expr a -> Expr a
plus g d = OpeBin   { bin  = Plus , exprg = g  , exprd = d }
 
moins :  Expr a ->  Expr a -> Expr a
moins  g d = OpeBin   { bin  = Moins , exprg = g  , exprd = d }
 

fois :  Expr a ->  Expr a -> Expr a
fois g d = OpeBin   { bin  = Fois , exprg = g  , exprd = d }
 
divi :  Expr a ->  Expr a -> Expr a
divi g d = OpeBin   { bin  = Div , exprg = g  , exprd = d }


puiss :  Expr a ->  Expr a -> Expr a
puiss g d = OpeBin   { bin  = Puiss , exprg = g  , exprd = d }
  
cosinus :   Expr a -> Expr a
cosinus g = OpeTrigo   { trigo  = Cos , expr = g  }

sinus :   Expr a -> Expr a
sinus g = OpeTrigo   { trigo  = Sin , expr = g  }


tangente :   Expr a -> Expr a
tangente g = OpeTrigo   { trigo  = Tan , expr = g  }



evalFold  : List (a, Float)  -> Expr a ->   Expr a
evalFold liste expr0 = 
        let
                eval : (a,Float) -> Expr a -> Expr a
                eval (var,val) exprE = 
                        case exprE of 
                                Const a -> Const a 
                                Inconnue xy-> if (xy == var) then Const val
                                        else Inconnue xy
                                OpeBin { bin,exprg,exprd}-> OpeBin{bin = bin, exprg = eval (var , val) exprg ,exprd = eval (var, val) exprd }
                                OpeTrigo  { trigo , expr } ->OpeTrigo {trigo = trigo, expr = eval (var,val) expr}
        in List.foldl  eval expr0 liste




calcul :  Expr a ->    Float
calcul   expr0 =
        case expr0 of 
                Const a -> a
                Inconnue xy-> 0
                OpeBin { bin,exprg,exprd}  -> 
                        case bin of 
                                Plus -> ( calcul exprg ) + ( calcul exprd)
                                Moins -> ( calcul exprg ) - ( calcul exprd)
                                Fois -> ( calcul exprg ) * ( calcul exprd)
                                Div -> ( calcul exprg ) / ( calcul exprd)
                                Puiss -> ( calcul exprg ) ^ ( calcul exprd)
                OpeTrigo  { trigo , expr } ->
                        case trigo of
                                Cos -> cos (calcul expr )
                                Sin -> sin (calcul expr)
                                Tan -> tan (calcul expr)




--(3 + x ) * (x - y)
exprtest : Expr Var 
exprtest =  fois (plus (Const 3 ) (Inconnue X))  (moins (Inconnue X)  ( Inconnue Y))

r :Float
r = calcul (evalFold [(X,4),(Y, 3)] exprtest)

-- (Cos X)^2 + (Sin Y)^2
expr2 : Expr Var
expr2  = (puiss (cosinus (Inconnue X )) (Const 2)) |> plus (puiss (sinus (Inconnue Y))(Const 2))

r2 : Float
r2 = calcul (evalFold [(X,2), (Y,2)] expr2)
