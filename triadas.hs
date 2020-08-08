{-
*Main> [ (n, sacaTriada n D) | n <- cutEscalaMayor D]
[(D,[D,Gb,A]),(E,[E,G,B]),(Gb,[Gb,A,Db]),(G,[G,B,D]),(A,[A,Db,E]),(B,[B,D,Gb]),(Db,[Db,E,G]),(D,[D,Gb,A])]
*Main> cutEscalaMayor D
[D,E,Gb,G,A,B,Db,D]

-}
data Note = A | B | C | D | E | F |G | Ab  | Bb | Db | Eb | Gb deriving (Show, Eq)
-- bemoles

escalaCromatica :: [Note]
escalaCromatica = [C,Db,D, Eb, E, F, Gb, G, Ab, A, Bb, B ] <> escalaCromatica
                -- 0 1  2  3   4  5  6   7  8   9 10   11  ...

creaEscala :: [Int] -> [a] -> [a]
creaEscala posiciones escala  = foldr (<>) aux $ repeat aux
  where aux = [escala !! i | i <- posiciones]


mayor :: [Int]
mayor = [0,2,4,5,7,9,11]

indiceRelativo n escala = faux n 0 escala
  where
    faux n i (x:xs) 
      | n == x = i
      | otherwise = faux n (i+1) xs

indiceRelativoCromatico :: Note -> Int
indiceRelativoCromatico n = faux n 0 escalaCromatica
  where
    faux n i (x:xs) 
      | n == x = i
      | otherwise = faux n (i+1) xs

triada:: [Int]
triada = [0,2,4]

escalaMayor n = creaEscala mayor $ drop (indiceRelativoCromatico n) escalaCromatica 

cutEscalaMayor n  = take 8 $ escalaMayor n


sacaTriada n e = take 3 $ creaEscala triada $ drop (indiceRelativo n miEscala) miEscala
  where miEscala =  escalaMayor e


todoAcordes nota = [ (n, sacaTriada n nota) | n <- cutEscalaMayor nota]
