module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float -> a) -> (Float -> Float -> a) -> (Expr -> a -> Expr -> a -> a)-> (Expr -> a -> Expr -> a -> a)-> (Expr -> a -> Expr -> a -> a)-> (Expr -> a -> Expr -> a -> a) -> Expr -> a
recrExpr cCosts cRango cSuma cResta cMult cDiv expr = case expr of
          Const i         -> cCosts i
          Rango x y       -> cRango x y
          Suma exp1 exp2  -> cSuma exp1 (rec exp1) exp2 (rec exp2)
          Resta exp1 exp2 -> cResta exp1 (rec exp1) exp2 (rec exp2)
          Mult exp1 exp2  -> cMult exp1 (rec exp1) exp2 (rec exp2)
          Div exp1 exp2   -> cDiv exp1 (rec exp1) exp2 (rec exp2)
    where
      rec = recrExpr cCosts cRango cSuma cResta cMult cDiv

-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) ->(a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr cCosts cRango cSuma cResta cMult cDiv expr = case expr of
          Const i         -> cCosts i
          Rango x y       -> cRango x y
          Suma exp1 exp2  -> cSuma (rec exp1) (rec exp2)
          Resta exp1 exp2 -> cResta (rec exp1) (rec exp2)
          Mult exp1 exp2  -> cMult (rec exp1) (rec exp2)
          Div exp1 exp2   -> cDiv (rec exp1) (rec exp2)
    where
      rec = foldExpr cCosts cRango cSuma cResta cMult cDiv

reutilizarGen :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
reutilizarGen op rec1 rec2 g = let (x1, g1) = rec1 g
                                   (x2, g2) = rec2 g1
                                in (op x1 x2, g2)

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr
  (,)
  (\x y g -> dameUno (x,y) g)
  (reutilizarGen (+))
  (reutilizarGen (-))
  (reutilizarGen (*))
  (reutilizarGen (/))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
