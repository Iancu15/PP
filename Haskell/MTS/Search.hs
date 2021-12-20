{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Data.List
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}
data Node s a = Nod s (Maybe a) (Maybe (Node s a)) Int Float [Node s a]

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Nod state1 _ _ _ _ _) == (Nod state2 _ _ _ _ _) =
        state1 == state2

instance Ord s => Ord (Node s a) where
    (Nod state1 _ _ _ _ _) <= (Nod state2 _ _ _ _ _) =
        state1 <= state2

instance (Show s, Show a) => Show (Node s a) where
    show (Nod state act prnt dpth hrst chldrn) =
        "state = " ++ show state ++
        " act = " ++ show act ++
        " parent = " ++ show prnt ++
        " depth = " ++ show dpth ++
        " heuristic = " ++ show hrst ++
        " number of children = " ++ show (length chldrn)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Nod state _ _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Nod _ _ prnt _ _ _) = prnt

nodeDepth :: Node s a -> Int
nodeDepth (Nod _ _ _ dpth _ _) = dpth

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Nod _ _ _ _ _ chldrn) = chldrn

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Nod _ _ _ _ hrst _) = hrst

nodeAction :: Node s a -> Maybe a
nodeAction (Nod _ act _ _ _ _) = act

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

{-
Creaza nodul in functie de act, stare, parinte si adancime, euristica este
calculata in functie de stare, iar copiii vor fi generati in interiorul functiei
-}
createNode :: (ProblemState s a, Eq s) => (Maybe a, s) -> Maybe (Node s a) -> Int -> Node s a
createNode (act, state) prnt dpth = nod where
    nod = Nod state act prnt dpth (h state) chldrn
    chldrn = map (\(succAct, succState) ->
        createNode (Just succAct, succState) (Just nod) (dpth + 1))
        (successors state)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createNode (Nothing, initialState) Nothing 0

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => PQ.PSQ k p -> (k, PQ.PSQ k p)
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

{-
Nodurile succesor nevizitate sunt nodurile copii a caror stare nu face parte din
din lista visited
-}
suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> S.Set s -> [Node s a]
suitableSuccs node visited = filter (\chld -> nodeState chld `notElem` visited) chldrn where
    chldrn = nodeChildren node

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

{-
Costul nodului va fi suma dintre euristica si adancime
Folosesc functia insertWith ce va primi perechea cheie -> nodul, valoarea -> costul alaturi
de o functie de combinare care pentru cazul in care nodul deja exista in coada si costul intrarii
e mai mic decat cel al valorii din PQ, atunci inlocuieste valoarea
-}
insertSucc :: (ProblemState s a, Ord s) => PQ.PSQ (Node s a) Float -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith combiningFunc node nodeCost frontier where
    combiningFunc nodeCost1 nodeCost2
        | nodeCost1 > nodeCost2 = nodeCost2
        | otherwise = nodeCost1
    nodeCost = nodeHeuristic node + fromIntegral (nodeDepth node) :: Float

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

{-
Folosesc foldl adaugand in frontiera cu insertSucc nodurile succesori care sunt suitable
-}
insertSuccs :: (ProblemState s a, Ord s) => Node s a -> PQ.PSQ (Node s a) Float -> S.Set s -> PQ.PSQ (Node s a) Float
insertSuccs node frontier visited = foldl insertSucc frontier (suitableSuccs node visited)

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

{-
Scot nod-ul minim din frontiera si creez noua lista de visited care va fi vechea lista de visited
la care se adauga nod-ul minim
Noua frontiera va fi vechea frontiera la care se adauga succesori nodului minim
Se continua recursiv pana se atinge scopul
-}
astar' :: (ProblemState s a, Ord s) => S.Set s -> PQ.PSQ (Node s a) Float -> Node s a
astar' visited frontier = finalState where
    (minNode, updatedFrontier) = deleteFindMin frontier
    state = nodeState minNode
    newVisited = S.insert state visited
    newFrontier = insertSuccs minNode updatedFrontier newVisited
    finalState
        | isGoal state = minNode
        | otherwise = astar' newVisited newFrontier

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = finalState where
    state = nodeState initialNode
    heuristic = nodeHeuristic initialNode
    visited = S.insert state S.empty
    frontier = PQ.insert initialNode heuristic PQ.empty
    finalState = astar' visited frontier

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

{-
Pentru fiecare nod din path, daca are parinte (x -> Just a), atunci pentru actiunea
sa (Just b) se adauga in lista ce se formeaza de unfoldr tuplul format din actiunea b
si starea nodului, se continua pe nodul parinte, se opreste cand se determina toata
calea
Obs: Nodul initial nu are parinte si astfel nu va ajunge pe cazul (Just a) si nu va
fi adaugat in lista
-}
extractPath :: Node s a -> [(a, s)]
extractPath goalNode = reverse $ unfoldr (\x -> case x of
    Just a -> case nodeAction a of
        Just b -> Just((b, nodeState a), nodeParent a)
        _      -> Nothing
    _      -> Nothing)
    (Just goalNode)