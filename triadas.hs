

data Note = A | B | C | D | E | F |G | Ab  | Bb | Db | Eb | Gb deriving (Show)
-- bemoles

escalaCromatica :: [Note]
escalaCromatica = [C,Db,D, Eb, E, F, Gb, G, Ab, A, Bb, B ] <> escalaCromatica
                -- 0 1  2  3   4  5  6   7  8   9 10   11  ...

creaEscala :: [Int] -> [a] -> [a]
creaEscala posiciones escala  = foldr (<>) aux $ repeat aux
  where aux = [escala !! i | i <- posiciones]

mayor :: [Int]
mayor = [0,2,4,5,7,9,11]


{-
*Main> drop 3 $ take 10 $ creaEscala mayor escalaCromatica 
[F,G,A,B,C,D,E]

-}
