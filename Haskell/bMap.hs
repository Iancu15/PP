import Data.List

{-
	Problema urmărește lucrul cu un dicționar / map bidirecțional, în sensul că atât cheile, cât și valorile sunt unice în întregul dicționar.
-}

{-
	1a. (3p) Definiți tipul de date.
-}
data BidirectionalMap a b = BidirectionalMap [(a, b)]

{-
	1b. (2p) Definiți dicționarul gol.
-}

emptyBM :: BidirectionalMap a b
emptyBM = BidirectionalMap []


{-
	1c. (5p) Implementați funcția getVal care caută valoarea corespunzătoare cheii date.
-}
getVal :: Eq a => a -> BidirectionalMap a b -> Maybe b
getVal key (BidirectionalMap bimap) = lookup key bimap

{-
	2a. (5p) Implementați funcția getKey care caută cheia corespunzătoare valorii date.
-}
getKey :: Eq b => b -> BidirectionalMap a b -> Maybe a
getKey value (BidirectionalMap bimap) = lookup value $ map (\(x, y) -> (y, x)) bimap

{-
	2b. (5p) Implementați funcția insBM care inserează o nouă corespondență în dicționar. Nu sunt inserate corespondențele care conțin o cheie sau o valoare deja existentă în dicționar.
-}
insBM :: (Eq b, Eq a) =>
     a -> b -> BidirectionalMap a b -> BidirectionalMap a b
insBM key value bimapStruct@(BidirectionalMap bimap) = case getVal key bimapStruct of
    Just _ -> BidirectionalMap bimap
    Nothing -> case getKey value bimapStruct of
        Just _ -> BidirectionalMap bimap
        Nothing -> BidirectionalMap ((key, value):bimap)

{-
	3. (10p) Instanțiați clasa Show astfel încât afișarea unui dicționar bidirecțional să prezinte atât corespondența de la chei la valori, cât și cea de la valori la chei. Corespondențele trebuie sortate lexicografic.
	Exemplu: > insBM 3 'a' $ insBM 2 'b' $ insBM 1 'c' emptyBM 
		'a' -> 3; 'b' -> 2; 'c' -> 1; 1 -> 'c'; 2 -> 'b'; 3 -> 'a'; 
-}

instance (Show a, Show b) => Show (BidirectionalMap a b) where
    show (BidirectionalMap bimap) = concatMap (\(x, y) -> show x ++ " -> " ++ show y ++ "; ") (map (\(x, y) -> (y, x)) bimap) ++
        concatMap (\(x, y) -> show x ++ " -> " ++ show y ++ "; ") bimap