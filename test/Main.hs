module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 0 "" ~?= "",
      alinearDerecha 1 "" ~?= " ",
      alinearDerecha 1 "a" ~?= "a",
      alinearDerecha 0 "incierticalc" ~?= "incierticalc",
      alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 6 "a b c" ~?= " a b c",
      alinearDerecha 6 " hola" ~?= "  hola"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem (+ 10) [1, 2, 3] 0          ~?= [11, 2, 3],
      actualizarElem (+ 10) [1, 2, 3] 1          ~?= [1, 12, 3],
      actualizarElem (+ 10) [1, 2, 3] 2          ~?= [1, 2, 13],
      actualizarElem (+ 10) [1, 2, 3] 3          ~?= [1, 2, 3],
      actualizarElem id [1,2,3] 1                ~?= [1,2,3],
      actualizarElem (+ 10) [1, 2, 3] 100        ~?= [1, 2, 3],
      actualizarElem (+ 10) [1, 2, 3] (-10)      ~?= [1, 2, 3],
      actualizarElem (*10) [1] 0                 ~?= [10],
      actualizarElem (*10) [] 0                  ~?= [],
      actualizarElem reverse ["Hola", "Mundo", "Que", "Tal"]  2  ~?= ["Hola","Mundo","euQ","Tal"]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 4 (-4, 4))
        ~?= [ Casillero infinitoNegativo (-4.0) 0 0.0,
              Casillero (-4.0) (-2.0) 0 0.0,
              Casillero (-2.0) 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 infinitoPositivo 0 0.0
            ],
      casilleros (vacio 5 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 0.2 0 0.0,
              Casillero 0.2 0.4 0 0.0,
              Casillero 0.4 0.6 0 0.0,
              Casillero 0.6 0.8 0 0.0,
              Casillero 0.8 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      -- largo de la lista de casilleros
      length (casilleros (vacio 1 (0,10))) ~?= 3,
      length (casilleros (vacio 3 (0,6))) ~?= 5,
      length (casilleros (vacio 4 (-4,4))) ~?= 6,
      length (casilleros (vacio 5 (0,1))) ~?= 7
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
      h1 = agregar (-1) h0
      h2 = agregar 0 h1
      h3 = agregar 2 h2
      h4 = agregar 4 h3
      h5 = agregar 6 h4
   in test
        [ casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 4 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100, -- El 100% de los valores están acá
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 6 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100 -- El 100% de los valores están acá
                ],
          casilleros h2
            ~?= [ Casillero infinitoNegativo 0 1 50,
                  Casillero 0 2 1 50,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros h3
            ~?= [ Casillero infinitoNegativo 0 1 (100/3),
                  Casillero 0 2 1 (100/3),
                  Casillero 2 4 1 (100/3),
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros h4
            ~?= [ Casillero infinitoNegativo 0 1 25,
                  Casillero 0 2 1 25,
                  Casillero 2 4 1 25,
                  Casillero 4 6 1 25,
                  Casillero 6 infinitoPositivo 0 0
                ],
           casilleros h5
            ~?= [ Casillero infinitoNegativo 0 1 (100/5),
                  Casillero 0 2 1 (100/5),
                  Casillero 2 4 1 (100/5),
                  Casillero 4 6 1 (100/5),
                  Casillero 6 infinitoPositivo 1 (100/5)
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [] ~?= vacio 4 (1, 5),
      histograma 4 (1, 5) [0] ~?= agregar 0 (vacio 4 (1, 5)),
      histograma 4 (1, 5) [1] ~?= agregar 1 (vacio 4 (1, 5)),
      histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 4 (1, 5) [5] ~?= agregar 5 (vacio 4 (1, 5)),
      casilleros (histograma 4 (1,5) [1,2,3]) ~?= casilleros (histograma 4 (1,5) [3,1,2]),
      histograma 4 (1,5) [-10, 0, 1, 4.9, 5, 7, 100]
        ~?= agregar 100 (agregar 7 (agregar 5 (agregar 4.9 (agregar 1 (agregar 0 (agregar (-10) (vacio 4 (1,5)))))))),
      histograma 4 (1,5) [1.1,1.2,1.3]
        ~?= agregar 1.3 (agregar 1.2 (agregar 1.1 (vacio 4 (1,5))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (agregar (-1) (vacio 3 (0,6)))
        ~?= [ Casillero infinitoNegativo 0 1 100,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (agregar 0 (vacio 3 (0,6)))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 1 100,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 4 (agregar 2 (agregar 0 (agregar (-1) (vacio 3 (0,6))))))
        ~?= [ Casillero infinitoNegativo 0 1 25,
              Casillero 0 2 1 25,
              Casillero 2 4 1 25,
              Casillero 4 6 1 25,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (agregar 6 (agregar 4 (agregar 2 (agregar 0 (agregar (-1) (vacio 3 (0,6)))))))
        ~?= [ Casillero infinitoNegativo 0 1 (100/5),
              Casillero 0 2 1 (100/5),
              Casillero 2 4 1 (100/5),
              Casillero 4 6 1 (100/5),
              Casillero 6 infinitoPositivo 1 (100/5)
            ]
    ]

-- Funciones auxiliares para testear recrExpr
reconstruir :: Expr -> Expr
reconstruir =
  recrExpr
    Const
    Rango
    (\e1 _ e2 _ -> Suma e1 e2)
    (\e1 _ e2 _ -> Resta e1 e2)
    (\e1 _ e2 _ -> Mult e1 e2)
    (\e1 _ e2 _ -> Div  e1 e2)

listaDeSubexpresiones :: Expr -> [Expr]
listaDeSubexpresiones =
  recrExpr
    (\x -> [Const x])
    (\a b -> [Rango a b])
    (\e1 l e2 r -> Suma e1 e2 : (l ++ r))
    (\e1 l e2 r -> Resta e1 e2 : (l ++ r))
    (\e1 l e2 r -> Mult e1 e2 : (l ++ r))
    (\e1 l e2 r -> Div  e1 e2 : (l ++ r))

testsRecr :: Test
testsRecr =
  test
    [ reconstruir (Const 5) ~?= Const 5,
      reconstruir (Rango 1 2) ~?= Rango 1 2,
      listaDeSubexpresiones (Const 5) ~?= [Const 5],
      listaDeSubexpresiones (Rango 1 2) ~?= [Rango 1 2],
      let e = Suma (Const 1) (Const 2)
        in reconstruir e ~?= e,
      let e = Suma (Const 1) (Mult (Const 2) (Const 3))
        in reconstruir e ~?= e,
      let e = Div (Suma (Rango 1 2) (Mult (Const 3) (Rango 10 11))) (Resta (Const 4) (Const 5))
        in reconstruir e ~?= e,
      let e = Suma (Const 1) (Const 2)
        in listaDeSubexpresiones e ~?= [e, Const 1, Const 2],
      let e = Suma (Const 1) (Mult (Const 2) (Const 3))
        in listaDeSubexpresiones e ~?= [e, Const 1, Mult (Const 2) (Const 3), Const 2, Const 3],
      let e = Div (Suma (Rango 1 2) (Mult (Const 3) (Rango 10 11))) (Resta (Const 4) (Const 5))
        in listaDeSubexpresiones e ~?= [e, Suma (Rango 1 2) (Mult (Const 3) (Rango 10 11)), Rango 1 2,
                                        Mult (Const 3) (Rango 10 11), Const 3, Rango 10 11, Resta (Const 4) (Const 5), Const 4, Const 5],
      let e = Mult (Const 1) (Resta (Const 2) (Const 3))
        in head (listaDeSubexpresiones e) ~?= e,
      let e = Div (Const 1) (Div (Const 2) (Const 3))
        in length (listaDeSubexpresiones e) ~?= 5,

     -- cuenta la cantidad de constantes en la expresión
      recrExpr (\_ -> 1 :: Int)   (\_ _ -> 0 :: Int) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (Resta (Const 5) (Const 6)) ~?= 2,

      -- suma los valores de las constantes de la expresión
      recrExpr id (\_ _ -> 0 ) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (\_ l _ r -> l + r) (Resta (Const 5) (Const 6)) ~?= 11.0

    ]

-- Funciones auxiliares para testear foldExpr
cantConst :: Expr -> Int
cantConst =
  foldExpr
    (const 1)
    (\_ _ -> 0)
    (+) (+) (+) (+)

altura :: Expr -> Int
altura =
  foldExpr
    (const 1)
    (\_ _ -> 1)
    (\l r -> 1 + max l r)
    (\l r -> 1 + max l r)
    (\l r -> 1 + max l r)
    (\l r -> 1 + max l r)

testsFold :: Test
testsFold =
  test
    [ cantConst (Const 3) ~?= 1,
      cantConst (Rango 1 2) ~?= 0,
      altura (Const 5) ~?= 1,
      altura (Rango 0 1) ~?= 1,
      let e = Suma (Const 1) (Const 2)
        in cantConst e ~?= 2,
      let e = Suma (Const 1) (Mult (Const 2) (Const 3))
        in cantConst e ~?= 3,
      let e = Suma (Const 1) (Const 2)
        in altura e ~?= 2,
      let e = Suma (Const 1) (Mult (Const 2) (Const 3))
        in altura e ~?= 3,
      let e = Div (Suma (Rango 1 2) (Mult (Const 3) (Rango 10 11))) (Resta (Const 4) (Const 5))
        in (cantConst e, altura e) ~?= (3, 4)
    ]

testsEval :: Test
testsEval =
  test
    [
      -- test base
      fst (eval (Resta (Const 5) (Const 6)) (genNormalConSemilla 0)) ~?= -1.0,
      fst (eval (Mult (Const 5) (Const 6)) (genNormalConSemilla 0)) ~?= 30.0,
      fst (eval (Div (Const 5) (Const 6)) (genNormalConSemilla 0)) ~?= 0.8333333,

      fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,

      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,


      -- distributiva
      fst (eval (Mult (Mult (Rango 1 5) (Rango 6 10)) (Rango 2 6)) (genNormalConSemilla 0)) ~?= fst (eval (Mult (Rango 1 5) (Mult (Rango 6 10) (Rango 2 6))) (genNormalConSemilla 0)),

      -- asociativa
      fst (eval (Mult (Rango 1 5) (Mult (Rango 3 6) (Const 4))) genFijo) ~?= fst (eval (Mult (Mult (Rango 1 5) (Const 4)) (Rango 3 6)) genFijo),

      -- elemento neutro
      fst (eval (Suma (Rango 1 5) (Const 0)) genFijo) ~?= 3.0,
      fst (eval (Resta (Rango 1 5) (Const 0)) genFijo) ~?= 3.0,
      fst (eval (Mult (Rango 1 5) (Const 1)) genFijo) ~?= 3.0,
      fst (eval (Div (Rango 1 5) (Const 1)) genFijo) ~?= 3.0

    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [completar]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [completar]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
