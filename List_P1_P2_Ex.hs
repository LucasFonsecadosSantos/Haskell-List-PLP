----------------------------------------------------------------------------------
{--O fatorial duplo de um número natural n é o produto de todos os números
de 1 (ou 2) até n, contados de 2 em 2. Por exemplo, o fatorial duplo de
8 é 8*6*4*2 = 384 e o fatorial duplo de 7 é 7*5*3*1 = 105. Defina uma
função recursiva em Haskell para calcular o fatorial duplo de um número
positivo.
--}
fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = fat(n-2)*n
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
{-
Fornecidos três valores, a, b e c, escreva uma função em Haskell que
retorne quantos dos três são iguais. A resposta pode ser
3 (todos iguais), 2 (dois iguais e o terceiro diferente)
ou 0 (todos diferentes).
-}
amount_numbers_are_equals :: Int -> Int -> Int -> Int
amount_numbers_are_equals a b c | (a == b) && (b == c) = 3
                                | ((a == b) && (b /= c)) = 2
                                | ((a == c) && (a /= b)) = 2
                                | ((b == c) && (a /= c)) = 2
                                | otherwise = 0
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Crie uma função em Haskell que encontra o maior elemento de uma lista.
get_biggest_of_list :: [Int] -> Int
get_biggest_of_list [a] = a                                        
get_biggest_of_list (x:xs) | (x > get_biggest_of_list xs) = x    
                           | otherwise = get_biggest_of_list xs
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
{--
Defina uma função em Haskell que receba duas listas de números inteiros
A e B de mesmo tamanho e retorne uma terceira lista C, cujos elementos
correspondam à diferença dos elementos da lista A pelos elementos da
lista B (calcule a diferença elemento a elemento).
--}
function_04 :: [Int] -> [Int] -> [Int]
function_04 a b = invert (functionTmp a b []) ([])

functionTmp :: [Int] -> [Int] -> [Int] -> [Int]
functionTmp [][]_ = []
functionTmp (x:xs) (k:ks) c = functionTmp xs ks c++[k-x]

invert :: [Int] -> [Int] -> [Int]
invert [] _ = []
invert (x:xs) emptyList = invert xs emptyList++[x]
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- This function returns the integer elements list sum, received by the parameter
list_elements_sum :: [Int] -> Int
list_elements_sum [] = 0
list_elements_sum (x:xs) = x + list_elements_sum xs
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- This function calculing the MDC value by two integer numbers by Euclides algorithm.
mdc :: Int -> Int -> Int
mdc a b
    | a <= 0 = b
    | b <= 0 = a
    | otherwise = mdc (b) (mod a b)
----------------------------------------------------------------------------------