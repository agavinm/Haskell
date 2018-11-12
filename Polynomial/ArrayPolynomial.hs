-- *****************************************************************
-- File:   ArrayPolynomial.hs
-- Author: Andrés Gavín Murillo 716358
-- Author: Eduardo Gimeno Soriano 721615
-- Date:   May 2018
-- Coms:   TecProg - Prac. 5 - Haskell
--         Module ArrayPolynomial
-- *****************************************************************

module ArrayPolynomial where

import Data.List

-- Devuelve el monomio 'x' de grado 1.
x :: [Double]
x = [1.0, 0.0]

-- Devuelve el monomio 'c' de grado 0.
coef :: Double -> [Double]
coef c = [c]

-- Dadas dos listas de Doubles, devuelve la lista resultante de sumar las dos, 
-- recorriéndolas de izquierda a derecha.
rbadd :: [Double] -> [Double] -> [Double]
rbadd (x:xs) (y:ys) = x+y : rbadd xs ys
rbadd [] ys = ys
rbadd xs [] = xs

-- Devuelve el polinomio resultante de sumar dos polinomios.
badd :: [Double] -> [Double] -> [Double]
badd xs ys = reverse (rbadd (reverse xs) (reverse ys))
-- Ya que los polinomios son representados de derecha a izquierda, es decir, 
-- el elemento con mayor exponente está a la izquierda, es necesario invertir
-- el orden de los vectores que los representan para poder emplear la función 
-- 'rbadd'.

-- Devuelve el polinomio resultante de sumar la lista de polinomios.
padd :: [[Double]] -> [Double]
padd lp = foldl badd [0.0] lp

-- Devuelve el polinomio resultante de multiplicar un monomio por un polinomio.
mpmul :: [Double] -> [Double] -> [Double]
mpmul (x:xs) (y:ys) = x*y : mpmul (x:xs) ys
mpmul (x:xs) [] = xs

-- Devuelve el polinomio resultante de multiplicar dos polinomios.
bmul :: [Double] -> [Double] -> [Double]
bmul (x:xs) y = badd (mpmul (x : (map (* 0.0) xs)) y) (bmul xs y)
bmul [] y = []

-- Devuelve el polinomio resultante de multiplicar la lista de polinomios.
pmul :: [[Double]] -> [Double]
pmul (x:xs) = foldr bmul x xs

-- Devuelve el grado de un polinomio p.
grado :: [Double] -> Integer
grado (x:xs) = 1 + grado xs
grado [] = -1

-- Devuelve el resultado de evaluar un polinomio p para un valor x.
peval :: [Double] -> Double -> Double
peval (y:ys) x = y * (x ^ grado (y:ys)) + peval ys x
peval [] _ = 0

-- Devuelve el polinomio resultante de multiplicar el exponente por el 
-- coeficiente en cada elemento de un polinomio p.
pexco :: [Double] -> [Double]
pexco (x:xs) = (x * fromIntegral (grado (x:xs))) : pexco xs
pexco [] = []
-- fromIntegral() convierte Integer a Number (Integer, Double, Float).

-- Devuelve el polinomio resultante de derivar un polinomio p.
pderv :: [Double] -> [Double]
pderv [] = []
pderv p = init (pexco p)
