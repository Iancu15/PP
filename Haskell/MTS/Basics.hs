{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}

data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

type Hunter = Position
type Obstacle = Position
type Gateway = (Position, Position)
type Size = (Int, Int)

data Game = Game [Target] Hunter [Obstacle] [Gateway] Size deriving (Eq, Ord)
{-
    *** Optional *** 
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

{- 
Inlocuieste caracterul de pe pozitia pos, din jocul primit ca un string,
cu caracterul sign corespunzator entitatii
-}
replaceChar :: Char -> Int -> String -> Position ->  String
replaceChar sign columns str pos = take n str ++ [sign] ++ drop (n + 1) str where
    n = fst pos * columns + snd pos

{-
Primeste o lista de entitati(lista are doar un tip de entitate) si sign-ul corespunzator
entitatilor si le adauga la pozitiile corespunzatoare din joc
-}
addElements :: [Position] -> Char -> Int -> String -> String
addElements poss sign columns str = foldl (replaceChar sign columns) str poss

{-
Imparte jocul in linii de lungime c(nr coloane) despartite de newline
Obs: Functia splitAt deja intoarce un tuplu (linie, restul de sir) ramas asa ca
o pot da direct ca argument la Just pentru functia unfoldr care primeste
(element nou de adaugat, rest de procesat)
-}
splitString :: Int -> String -> [String]
splitString c = unfoldr (\x -> if null x
    then Nothing
    else Just (splitAt c x))

{-
Se creeaza intai string-ul aferent jocului ca fiind total gol si anume un sir de spatii
de lungime l * c
Apoi fiecare lista de entitati isi are entitatile adaugate in sir
In final se imparte sirul si se obtine o lista de linii peste care se foloseste functia
intercalate care uneste liniile punand newline intre ele
-}
gameAsString :: Game -> String
gameAsString (Game tgs hter obs gates (l, c)) =
    intercalate "\n" $ splitString c $ addElements (map position tgs) '*' c $
    addElements [hter] '!' c $ addElements obs '@' c $
    addElements (map fst gates ++ map snd gates) '#' c $ replicate (l * c) ' '

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

{-
Genereaza obstacolele de la periferie ce se regasesc in fiecare joc
Obs: pozitiile sunt indexate de la 0 asa ca se merge doar pana la (l - 1) si (c - 1) respectiv
-}
generateSideObstacles :: Int -> Int -> [Position]
generateSideObstacles l c =
    [(x, y) | x <- [0, l - 1], y <- [0..c - 1]] ++ [(x, y) | x <- [0..l - 1], y <- [0, c - 1]]

emptyGame :: Int -> Int -> Game
emptyGame l c = Game [] (1, 1) (generateSideObstacles l c) [] (l, c)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

{-
Verifica daca pozitia se afla in afara jocului, intoarce adevarat daca da
Obs: cazul in care x = l si c = y este in afara jocului datorita indexarii de la 0
-}
isPositionOutOfRange :: Position -> (Int, Int) -> Bool
isPositionOutOfRange (x, y) (l, c) = l <= x || c <= y || x < 0 || y < 0

{-
Daca pozitia este ocupata de un obstacol sau in afara jocului se pastreaza campul
htr neschimbat(fie ca era gol inainte sau avea un hunter pentru care se dorea
schimbarea pozitiei), altfel se schimba hunter-ul la pozitia specificata
-}
addHunter :: Position -> Game -> Game
addHunter pos (Game tgs htr obs gates size) =
    Game tgs newHtr obs gates size where
        newHtr
            | pos `elem` obs = htr
            | isPositionOutOfRange pos size = htr
            | otherwise = pos

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget bhvr pos (Game tgs htr obs gates size) =
    Game (Target pos bhvr:tgs) htr obs gates size

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway gateways (Game tgs htr obs gates size) =
    Game tgs htr obs (gateways:gates) size

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos (Game tgs htr obs gates size) =
    Game tgs htr (pos:obs) gates size

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos (Game _ _ obs gates size)
    | isPositionOutOfRange pos size = Nothing
    | pos `elem` obs = Nothing
    | isJust gatewayPartner1 = gatewayPartner1
    | isJust gatewayPartner2 = gatewayPartner2
    | otherwise = Just pos
    where
        gatewayPartner1 = lookup pos gates
        gatewayPartner2 = lookup pos $ map (\(a, b) -> (b, a)) gates

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

{-
Muta elementul din pozitia curenta initPos in o pozitie urmatoare dorita wantPos daca
este posibil, dau behavior-ul ca sa stiu cu ce behavior va ramane target-ul dupa
mutare
-}
goSomewhere :: Bool -> Behavior -> Position -> Behavior
goSomewhere withBounce bhvr initPos wantPos game = case attemptMove wantPos game of
    (Just a) -> Target a bhvr
    {-
    al doilea attemptMove e pentru a te intoarce in celalalt gateway daca nu ai bounce
    si nu mai ai unde sa mergi din gateway-ul curent cu behaviour-ul aferent
    -}
    _        -> if withBounce then Target initPos bhvr
                else case attemptMove initPos game of
                    (Just a) -> Target a bhvr
                    _        -> Target initPos bhvr {- <- cazul asta nu se va intampla vreodata -}

goEast :: Behavior
goEast pos@(x, y) = goSomewhere False goEast pos (x, y + 1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(x, y) = goSomewhere False goWest pos (x, y - 1)


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(x, y) = goSomewhere False goNorth pos (x - 1, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(x, y) = goSomewhere False goSouth pos (x + 1, y)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

{-
1 pentru nord, -1 pentru sud
Calculez pozitia la care ar ajunge daca ar merge in nord sau in sud
Daca nu s-a schimbat pozitia daca alegeam sa merg in aceeasi directie
atunci inseamna ca e blocata si schimb directia, acesta e motivul pentru
care goSomewhere nu se intoarce in celalalt gateway pentru cazul in care
nu te poti misca, pentru ca in situatia noastra ar trebui sa schimbam
directia, nu gateway-ul
Noua pozitie va fi pozitia corespunzatoarea urmatoarei directii (pentru
directia 1 va fi northPos, pentru directia -1 va fi southPos)
-}
bounce :: Int -> Behavior
bounce dir pos@(x, y) game
    | dir == 1 && dirNorth == 1  = Target northPos (bounce dirNorth)
    | dir == 1 && dirNorth == -1 = Target southPos (bounce dirNorth)
    | dir == -1 && dirSouth == 1 = Target northPos (bounce dirSouth)
    | otherwise                  = Target southPos (bounce dirSouth)
        where
            southPos = position (goSomewhere True goSouth pos (x + 1, y) game)
            northPos = position (goSomewhere True goNorth pos (x - 1, y) game)
            newDirection newPos
                | pos == newPos = -dir
                | otherwise = dir
            dirNorth = newDirection northPos 
            dirSouth = newDirection southPos

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game tgs htr obs gates size) =
    Game newTgs htr obs gates size where
        newTgs = map (\tg -> behavior tg (position tg) game) tgs

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}

{-
Pozitiile adiciente sunt x + 1, x - 1, y + 1, y - 1
-}
isTargetKilled :: Hunter -> Target -> Bool
isTargetKilled (hx, hy) tg
    | hx == tx = (hy == ty + 1) || (hy == ty - 1) 
    | hy == ty = (hx == tx + 1) || (hx == tx - 1)
    | otherwise = False
        where
            (tx, ty) = position tg

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    

    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

{-
Aplicand filter pe isTargetKilled, erau filtrati cei in viata, asa ca a trebuit
sa fac o functie opusa
-}
clearKilledTargets :: Game -> Game
clearKilledTargets (Game tgs htr obs gates size) =
    Game updatedTgs htr obs gates size where
        targetNotKilled hunter target = not (isTargetKilled hunter target)
        updatedTgs = filter (targetNotKilled htr) tgs

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir isReal game@(Game tgs htr obs gates size)
    | isReal = clearKilledTargets $ moveTargets $ clearKilledTargets moveHtrGame
    | otherwise = moveHtrGame
        where
            moveHtrGame = Game tgs newHtr obs gates size
            applyDir bhvr = position (bhvr htr game)
            newHtr
                | dir == North = applyDir goNorth
                | dir == South = applyDir goSouth
                | dir == West  = applyDir goWest
                | otherwise    = applyDir goEast

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game tgs _ _ _ _) = null tgs

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = zip [North, South, East, West] [north, south, east, west] where
        north = advanceGameState North False game
        south = advanceGameState South False game
        west  = advanceGameState West False game
        east  = advanceGameState East False game

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    {- Daca s-a omorat vreun target inseamna ca am ajuns la scop, nu imi pasa
    care a fost omorat, doar sa fi fost omorat cel putin unul -}
    isGoal (Game tgs htr _ _ _) = any (isTargetKilled htr) tgs

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}

    {- Am decis ca euristica sa fie distanta fata de cel mai apropiat target -}
    h (Game tgs htr _ _ _) = minimum $ map (hEuclidean htr . position) tgs

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
