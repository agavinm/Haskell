-- *****************************************************************
-- File:   TupleListPolynomial.hs
-- Author: Andrés Gavín Murillo 716358
-- Author: Eduardo Gimeno Soriano 721615
-- Date:   May 2018
-- Coms:   TecProg - Prac. 5 - Haskell
--         Module TupleListPolynomial
-- *****************************************************************

module TupleListPolynomial where

import Data.List

-- Devuelve el monomio 'x' de grado 1.
x :: [(Double, Integer)]
x = [(1.0, 1)]

-- Devuelve el monomio 'c' de grado 0.
coef :: Double -> [(Double, Integer)]
coef c = [(c, 0)]

-- Devuelve el polinomio resultante de sumar un monomio con un polinomio.
-- Si el polinomio a sumar está ordenado, el polinomio resultante estará
-- también ordenado.
mpbadd :: [(Double, Integer)] -> [(Double, Integer)] -> [(Double, Integer)]
mpbadd ((xb,xe):xs) ((yb,ye):ys)
 | xe == ye = (xb + yb, xe) : (mpbadd xs ys)
 | xe > ye = (xb, xe) : (mpbadd xs ((yb,ye):ys))
 | xe < ye = (yb, ye) : (mpbadd ((xb,xe):xs) ys)
mpbadd [] ys = ys
mpbadd xs [] = xs

-- Devuelve el polinomio ordenado resultante de la suma de la lista de monomios.
mppadd :: [[(Double, Integer)]] -> [(Double, Integer)]
mppadd (x:xs) = foldl mpbadd x xs
mppadd [] = []

-- Devuelve la lista de monomios de un polinomio p.
mono :: [(Double, Integer)] -> [[(Double, Integer)]]
mono ((xb,xe):xs) = [(xb,xe)] : mono xs
mono [] = []

-- Devuelve el polinomio resultante de quitar los elementos que valen 0.
quitz :: [(Double, Integer)] -> [(Double, Integer)]
quitz ((xb,xe):xs)
 | xb == 0 = quitz xs
 | otherwise = (xb,xe) : quitz xs
quitz [] = []

-- Devuelve un polinomio simplificado y ordenado a partir de un polinomio p.
simpl :: [(Double, Integer)] -> [(Double, Integer)]
simpl ls = quitz (mppadd (mono ls))

-- Devuelve el polinomio simplificado y ordenado resultante de sumar dos 
-- polinomios.
badd :: [(Double, Integer)] -> [(Double, Integer)] -> [(Double, Integer)]
badd x y = simpl (x++y)

-- Devuelve el polinomio simplificado y ordenado resultante de sumar la lista 
-- de polinomios.
padd :: [[(Double, Integer)]] -> [(Double, Integer)]
padd (x:xs) = foldl badd x xs
padd [] = []

-- Devuelve el polinomio resultante de multiplicar un monomio por un polinomio.
mpbmul :: [(Double, Integer)] -> [(Double, Integer)] -> [(Double, Integer)]
mpbmul ((xb,xe):xs) ((yb,ye):ys) = (xb * yb, xe + ye) : mpbmul ((xb,xe):xs) ys
mpbmul [] ys = []
mpbmul xs [] = []

-- Devuelve el polinomio simplificado y ordenado resultante de multiplicar dos 
-- polinomios.
bmul :: [(Double, Integer)] -> [(Double, Integer)] -> [(Double, Integer)]
bmul ((xb,xe):xs) y = badd (mpbmul [(xb,xe)] y) (bmul xs y)
bmul [] y = []

-- Devuelve el polinomio simplificado y ordenado resultante de multiplicar la 
-- lista de polinomios.
pmul :: [[(Double, Integer)]] -> [(Double, Integer)]
pmul (x:xs) = foldl bmul x xs
pmul [] = []

-- Devuelve el resultado de evaluar un polinomio p para un valor x.
peval :: [(Double, Integer)] -> Double -> Double
peval ((xb,xe):xs) x = xb * (x ^ xe) + peval xs x
peval [] _ = 0.0

-- Devuelve el polinomio simplificado y ordenado resultante de derivar un 
-- polinomio p.
pderv :: [(Double, Integer)] -> [(Double, Integer)]
pderv ((xb,xe):xs) = simpl ((xb * fromIntegral (xe), xe - 1) : pderv xs)
pderv [] = []
-- fromIntegral() convierte Integer a Number (Integer, Double, Float).
