--Q1

listEquals (x:xs)
    | xs == [] = True
    | otherwise = ((x == head(xs)) && listEquals xs)

--Q2

onList _ [] = False --se não houver mais nenhum elemento entao não existe esse elemento na lista
onList x ys
    | x == z = True -- se x e cabeça de ws são iguais entao true
    | otherwise = x `onList` ws --senão chame o proximo elemento
    where (z:ws) = ys

interSec [] _ = []
interSec xs ys
    | z `onList` ys = z:zs `interSec` ys --se o elemento i de xs está na lista de ys coloque-o na lista,
    | otherwise = zs `interSec` ys       --e chame recursivamente para o proximo elemento
    where (z:zs) = xs                    --senão chame o proximo elemento

--Q3

resto x y
    | y == 0 = 0
    | otherwise = x `mod` y

--Q4

doisElevadoaN n
    | n == 0 = 1
    | otherwise = 2 * (doisElevadoaN (n - 1))

pow x n
    | n <= 0 = 1
    | otherwise = (x * (pow x (n - 1)))

--Q5

doubleSum xs
    | xs == [] = 0
    | otherwise = (2 * (foldl1 (+) xs))

--Q6

bissexto n
    | n `mod` 400 == 0 = True
    | ((n `mod` 4) == 0) && ((n `mod` 100) > 0) = True
    | otherwise = False

--Q7

isDivisor x y -- x divide y?
    | x `resto` y == 0 = True
    | otherwise = False

divHelper x y
    | y == 0 = [] -- se o divisor for zero subistitui por um [] para construir a lista
    | isDivisor x y = y:x `divHelper` (y - 1) --se x divide y então x vai pra lista de divisores
    | otherwise = x `divHelper` (y - 1) -- se x nao divide y entao tente o proximo numero

divisor n = reverse(n `divHelper` n)

divis x n
    | x `mod` n == 0 = True
    | otherwise = False


divisorFilter n = (\x -> ((n `mod` x) == 0)) `filter` [1..n] --filtro sobre a lista

--Q8

takeZ _ [] = []
takeZ z ws
    | x == z = xs
    | otherwise = x:(z `takeZ` xs)
    where (x:xs) = ws

sameElements [] zs = zs == []
sameElements ws zs --verifica se as duas listas possuem os mesmos elementos nao importando a ordem
    | x `onList` zs = xs `sameElements` (x `takeZ` zs) --se x está na lista de zs
    | otherwise = False
    where (x:xs) = ws

--Q9

extensionReader "" = ""
extensionReader str
    | x == '.' = xs
    | otherwise = extensionReader xs
    where (x:xs) = str

--Q10

elementCounter x xs = length((\z -> (x == z)) `filter` xs)

--Q11

average :: Fractional a => [a] -> a
average xs = (sum xs) / (fromIntegral (length xs))
aboveAverage xs = length((\x -> x > (average xs)) `filter` xs)

--Q12

isUpper c --Verifica se a letra eh maiuscula
    | (c >= 'A' && c <= 'Z') = True
    | otherwise = False

nameFinderHelper "" = ""
nameFinderHelper xs --Pega os caracteres ate encontrar um espaco retornando uma lista de char
    | (x /= ' ') && (not(isUpper x)) = x:nameFinderHelper ws
    | otherwise = [] --ultimo elemento para construir a lista
    where (x:ws) = xs

nameFinder "" = []
nameFinder str
    | isUpper(x) = (x:nameFinderHelper (xs)):nameFinder xs --Se a func encontra uma letra maiuscula ele chama o namehelper para construir o nome
    | otherwise = nameFinder xs                           --chamando recursivamente o resto da string para construir os nomes
    where (x:xs) = str

--Q13
indexFinder [] _ = 0
indexFinder list elem
    | x == elem = 0
    | x /= elem = 1 + indexFinder xs elem
    where (x:xs) = list

--Q14

splitEvenOdd [] = ([],[])
splitEvenOdd list = ((filter odd list), (filter even list))

--Q15

isInCircumference (x, y) (a, b) r
    | (sqrt(((x - a)^2) + ((y - b)^2)) <= r) = True
    | otherwise = False

--Q16

isPrime n = length((\x -> n `rem` x == 0) `filter` [1 .. n]) == 2

primesLessThanN n = (\x -> isPrime(x)) `filter` [1 .. n]

--Q17
takeR [] _ = []
takeR list qnt = drop (length(list) - qnt) list
dropR [] _ = []
dropR list qnt = take (length(list) - qnt) list

isPalindrome [] = True
isPalindrome str
    | x == last(str) = isPalindrome (take (length xs - 1) (xs)) 
    | otherwise = False
    where (x:xs) = str

--Q18

breakSlash "" = ("","")
breakSlash str = (take (indexFinder str '/') str, drop (indexFinder str '/' + 1) str)

--Q19
compareList [] [] = True
compareList (x:xs) (y:ys) = x == y && compareList xs ys

replaceWord frase palavra subst
    | (x == y) && (compareList palavra (take (length palavra) frase)) = subst ++ drop (length palavra) frase
    | otherwise = y:replaceWord ys palavra subst
    where (x:xs) = palavra; (y:ys) = frase

--Q20

map3 f xs ys zs = [(f x, f y, f z) | x <- xs, y <- ys, z <- zs]

--Q21
expressionAvaliator [] [] = True
expressionAvaliator [] _ = False
expressionAvaliator str stack
    | (x `onList` "{[(") = expressionAvaliator xs (x:stack)
    | (x `onList` ")]}" && stack /= []) = ((s:[x]) `onList` ["()", "[]", "{}"]) && (expressionAvaliator xs st)
    | (x `onList` ")]}" && stack == []) = False
    | otherwise = expressionAvaliator xs stack
    where (x:xs) = str; (s:st) = stack

