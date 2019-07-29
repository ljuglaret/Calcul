## Objectif
Le but est de calculer des expressions arithmétiques telles que (X + Y )^2 + (X - Z)*4, ou (cos 4) + (sin 2).
Pour cela if faut définir le type expression **Expr**. Celui ci peut être composé de constantes (1.6, 2.4,-5,-2.3) de variables (que l on peut nommer x,y,teta,omaga), et bien sur d opérations arithmétiques.
 
Les opérations arithmétiques peuvent prendre deux arguments, comme c est le cas de la multiplication, l'addition, la soustraction, la division.


```elm
type Bin = Plus 
             | Moins 
             | Fois 
             | Div      
             | Puiss    
```


Ou n'avoir qu'un seul argument comme c'est le cas des fonctions trigonométriques (cosinus, sinus, tangente).

```elm 

type Trigo =  Cos       
            | Sin      
            | Tan      

```

Au final on obtient le type algébrique suivant :
```elm

type Expr a = OpeBin   { bin  : Bin , exprg : Expr a, exprd : Expr a}
            | Const     Float
            | Inconnue  a
            | OpeTrigo  { trigo : Trigo, expr : Expr a} 

type Var = X  
            | Y  
            | Z   
```

## Définir les opérations
Les opérations seront définies selon ce modèle

```elm
plus :  Expr a ->  Expr a -> Expr a
plus g d = OpeBin   { bin  = Plus , exprg = g  , exprd = d }
```

Ou : 

```elm    
cosinus :   Expr a -> Expr a
cosinus g = OpeTrigo   { trigo  = Cos , expr = g  }
```

## Evaluation

Il faut ensuite pouvoir évaluer une expression en une liste de points.   
Par exemple : X^2 + Y^2 en (X,Y) = (2,3)

```elm

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
```
## Calcul

Enfin, le calcul de cette expression :    

```elm   
calcul :  Expr a ->    Float
calcul   expr0 =
        case expr0 of 
                Const a -> a
                Inconnue xy-> 0
                OpeBin { bin,exprg,exprd}  -> 
                        case bin of 
                                Plus -> ( calcul exprg ) + ( calcul exprd)
                                .
                                .
                                .
                OpeTrigo  { trigo , expr } ->
                        case trigo of
                                Cos -> cos (calcul expr )
                                .
                                .
```
