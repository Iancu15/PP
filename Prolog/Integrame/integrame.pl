:- ensure_loaded('checker.pl').

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
get_qstruct(Pos, (Text, Dir, Id), (Pos, Text, Dir, Id)).
cell((Pos, [Q1, Q2]), [QStruct1, QStruct2]) :-
    get_qstruct(Pos, Q1, QStruct1), get_qstruct(Pos, Q2, QStruct2), !.
cell((Pos, [(Text, Dir, Id)]), [(Pos, Text, Dir, Id)]) :- !.
cell(_, []).

process_qs([Cell | RestCells], ListQ) :-
    cell(Cell, CellQ),
    process_qs(RestCells, RestQList),
    append(CellQ, RestQList, ListQ).
process_qs([], []).

intrebari(integ(_, _, Cells, _), ListQ) :- process_qs(Cells, ListQ).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_question_list([(_, QText, _, QId) | _], QText, QId) :- !.
id_question_list([_ | Rest], QText, QId) :- id_question_list(Rest, QText, QId).

id_intrebare(Integ, QText, QId) :- intrebari(Integ, ListQ),
                                    id_question_list(ListQ, QText, QId).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
find_question([(Pos, Text, Dir, Id) | _], Text, (Pos, Text, Dir, Id)) :- !.
find_question([_ | RestListQ], Text, QStruct) :- find_question(RestListQ, Text, QStruct).

%add_char([((X, Y), _) | RestCells], (X, Y), Char, [((X, Y), Char) | RestCells]) :- !.
%add_char([Cell | RestCells], Pos, Char, [Cell | ResRestCells]) :- add_char(RestCells, Pos, Char, ResRestCells).
%add_char([], _, _, []).

get_new_pos((X, Y), Dir, (NewX, Y)) :- Dir = j, !, NewX is (X + 1).
get_new_pos((X, Y), Dir, (X, NewY)) :- Dir = d, NewY is (Y + 1).

find_cell([(Pos, Value) | _], Pos, Value) :- !.
find_cell([_ | Cells], Pos, Value) :- find_cell(Cells, Pos, Value).

add_char(Cells, Pos, _, Cells) :- find_cell(Cells, Pos, _), !.
add_char(Cells, Pos, Char, [(Pos, Char) | Cells]).

add_answer(Cells, Pos, Dir, [Char | Chars], ResCells) :-
    get_new_pos(Pos, Dir, NewPos),
    add_char(Cells, NewPos, Char, ResCellsAdd),
    add_answer(ResCellsAdd, NewPos, Dir, Chars, ResCells).
add_answer(Cells, _, _, [], Cells).

fill(Cells, ListQ, [(QText, Ans) | RestSol], ResCells) :-
    find_question(ListQ, QText, (Pos, _, Dir, _)),
    atom_chars(Ans, Chars),
    add_answer(Cells, Pos, Dir, Chars, ResCellsAdd),
    fill(ResCellsAdd, ListQ, RestSol, ResCells).
fill(Cells, _, [], Cells).

completare(integ(H, W, Cells, Vocab), Sol, integ(H, W, ResCells, Vocab)) :-
    process_qs(Cells, ListQ),
    fill(Cells, ListQ, Sol, ResCells).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
calculate_length(Cells, Pos, Dir, 0) :-
    get_new_pos(Pos, Dir, NewPos),
    find_cell(Cells, NewPos, _), !.
calculate_length(Cells, Pos, Dir, ResLen) :-
    get_new_pos(Pos, Dir, NewPos),
    calculate_length(Cells, NewPos, Dir, Len),
    ResLen is (Len + 1).

lungime_spatiu(integ(_, _, Cells, _), QText, Len) :-
    process_qs(Cells, ListQ),
    find_question(ListQ, QText, (Pos, _, Dir, _)),
    calculate_length(Cells, Pos, Dir, Len).



% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
replicate(_, 0, []) :- !.
replicate(Char, Len, [Char | Chars]) :-
    Sub1Len is (Len - 1), replicate(Char, Sub1Len, Chars).

find_intersection_cell(Cells, Pos, Dir, _, CellPos) :-
    get_new_pos(Pos, Dir, CellPos),
    find_cell(Cells, CellPos, Value), Value \= 'A', !, false.
find_intersection_cell(Cells, Pos, Dir, Value, CellPos) :-
    get_new_pos(Pos, Dir, CellPos),
    find_cell(Cells, CellPos, Value), Value = 'A', !.
find_intersection_cell(Cells, Pos, Dir, Value, CellPos) :-
    get_new_pos(Pos, Dir, NewPos),
    find_intersection_cell(Cells, NewPos, Dir, Value, CellPos).

calculate_index((CX, _), Dir, (QX, _), Index) :- Dir = j, !, Index is (CX - QX - 1).
calculate_index((_, CY), Dir, (_, QY), Index) :- Dir = d, Index is (CY - QY - 1).

intersectie(integ(H, W, Cells, V), Q1, Index1, Q2, Index2) :-
    lungime_spatiu(integ(H, W, Cells, V), Q1, Len),
    replicate('A', Len, DummyChars),
    process_qs(Cells, ListQ),
    find_question(ListQ, Q1, (Pos1, _, Dir1, _)),
    add_answer(Cells, Pos1, Dir1, DummyChars, ResCells),
    find_question(ListQ, Q2, (Pos2, _, Dir2, _)),
    find_intersection_cell(ResCells, Pos2, Dir2, 'A', CellPos),
    calculate_index(CellPos, Dir1, Pos1, Index1),
    calculate_index(CellPos, Dir2, Pos2, Index2).


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
get_solutions([Word | RestVocab], Len, [WChars | Sol]) :-
    atom_chars(Word, WChars),
    length(WChars, Len), !,
    get_solutions(RestVocab, Len, Sol).
get_solutions([_ | RestVocab], Len, Sol) :-
    get_solutions(RestVocab, Len, Sol).
get_solutions([], _, []).

sol_for_qs(Integ, [(_, QText, _, _) | ListQ], Vocab, [(QText, Sol) | RestSol]) :-
    lungime_spatiu(Integ, QText, Len),
    get_solutions(Vocab, Len, Sol),
    sol_for_qs(Integ, ListQ, Vocab, RestSol).
sol_for_qs(_, [], _, []).

solutii_posibile(integ(H, W, Cells, Vocab), Sol) :-
    process_qs(Cells, ListQ),
    sol_for_qs(integ(H, W, Cells, Vocab), ListQ, Vocab, Sol).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
check_validity_of_pair(Integ, QText1, QText2, SolElem1, SolElem2) :-
    intersectie(Integ, QText1, Index1, QText2, Index2), !,
    nth0(Index1, SolElem1, CommonLetter), nth0(Index2, SolElem2, CommonLetter).
check_validity_of_pair(_, _, _, _, _).

add_sol(_, [(QText, SolList)], [(QText, SolElemWord)]) :-
    !, member(SolElemChars, SolList),
    atom_chars(SolElemWord, SolElemChars).
add_sol(Integ, [(QText1, SolList1) | Rest], [(QText1, SolElem1Word) | RestSol]) :-
    add_sol(Integ, Rest, RestSol), member(SolElem1Chars, SolList1),
    atom_chars(SolElem1Word, SolElem1Chars), \+member((_, SolElem1Word), RestSol),
    forall((member((QText2, SolElem2Word), RestSol), atom_chars(SolElem2Word, SolElem2Chars)),
            check_validity_of_pair(Integ, QText1, QText2, SolElem1Chars, SolElem2Chars)).

reduce_possibilities(Integ, (QText1, [SolElem1 | SolList1]), RestQ, (QText1, [SolElem1 | ResSolList1])) :-
    forall(member((QText2, SolList2), RestQ),
        (findall(SolElem2, (member(SolElem2, SolList2),
            check_validity_of_pair(Integ, QText1, QText2, SolElem1, SolElem2)), L), length(L, Len), Len > 0)), !,
    reduce_possibilities(Integ, (QText1, SolList1), RestQ, (QText1, ResSolList1)).
reduce_possibilities(Integ, (QText1, [_ | SolList1]), RestQ, (QText1, ResSolList1)) :-
    reduce_possibilities(Integ, (QText1, SolList1), RestQ, (QText1, ResSolList1)).
reduce_possibilities(_, (_, []), _, (_, [])).

process_possibilities(Integ, AllPossibilities, [Possibility | PossibleSol], [PossibilityRes | PossibleSolRes]) :-
    process_possibilities(Integ, AllPossibilities, PossibleSol, PossibleSolRes),
    reduce_possibilities(Integ, Possibility, AllPossibilities, PossibilityRes).
process_possibilities(_, _, [], []).

rezolvare(Integ, Sol) :-
    solutii_posibile(Integ, PossibleSol),
    process_possibilities(Integ, PossibleSol, PossibleSol, PossibleSolRes),
    add_sol(Integ, PossibleSolRes, Sol).