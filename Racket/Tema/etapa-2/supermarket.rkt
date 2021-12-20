#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (get-index C)
  (match C
    [(counter index tt et queue)
     index]))

; aplic asupra fiecarui counter o functie
; lambda care il actualizeaza cu functia data ca parametru
; daca are index-ul egal cu cel primit ca parametru
(define (update-curry f index)
  (lambda (counters)
      (map (lambda (x)
          (if (= (get-index x) index)
              (f x)
              x))
       counters)))

(define (update f counters index) ((update-curry f index) counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (get-tt C)
  (match C
    [(counter index tt et queue)
     tt]))

(define (tt+ minutes)
    (lambda (C)
      (struct-copy counter C
               [tt (+ minutes
                      (get-tt C))])))

; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (get-et C)
  (match C
    [(counter index tt et queue)
     et]))

(define (et+ minutes)
    (lambda (C)
      (struct-copy counter C
               [et (+ minutes
                      (get-et C))])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define (get-queue C)
  (match C
    [(counter index tt et queue)
     queue]))

(define (add-to-counter name n-items)
  (lambda (C)
    ((tt+ n-items) (struct-copy counter C
                      [queue (append (get-queue C) (list (cons name n-items)))]))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

; Intoarce minimul dintre C1 si C2 conform definitiei
(define (min-counter C1 C2 get-elem)
  (cond
    ((< (get-elem C1) (get-elem C2)) C1)
    ((> (get-elem C1) (get-elem C2)) C2)
    (else (if (< (get-index C1) (get-index C2))
              C1
              C2))))

; Intoarce structura de tip counter minima din lista counters
(define (min-struct counters get-elem)
  (if (null? (cdr counters))
             (car counters)
             (min-counter (car counters) (min-struct (cdr counters) get-elem) get-elem)))

; Intoarce perechea de tip (index elem) aferenta minimului din lista counters
(define (min-elem counters get-elem)
  (define result (min-struct counters get-elem))
  (cons (get-index result) (get-elem result)))

(define (min-tt counters) (min-elem counters get-tt)) ; folosind funcția de mai sus
(define (min-et counters) (min-elem counters get-et)) ; folosind funcția de mai sus

; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.

; intoarce o copie a structurii C primita ca parametru in care
; se adauga minutes-tt la tt si minutes-et la et
(define (update-tt-et minutes-tt minutes-et)
  (lambda (C)
    ((et+ minutes-et) ((tt+ minutes-tt) C))))

; intoarce negatul unui numar primit ca parametru
(define (neg number)
  (- 0 number))

; elimina primul client
(define (new-C C)
  (struct-copy counter C
                      [queue (cdr (get-queue C))]))

(define (remove-first-from-counter C)
  ; daca n-are clienti, nu-i fac nimic
  ; daca ala era singurul client, atunci
  ; actualizez tt si et la 0 adaugand negatul respectiv
  ; altfel pentru tt sterg et-ul, iar pe et
  ; il resetez si adaug timpul de procesare a celui de-al
  ; doilea client
  (define queue (get-queue C))
  (if (null? queue)
      C
      (if (null? (cdr queue))
          ((update-tt-et (neg (get-tt C)) (neg (get-et C))) (new-C C))
          ((update-tt-et (neg (get-et C))
                            (+ (neg (get-et C)) (cdr (car (get-queue (new-C C))))))
           (new-C C)))))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)

; daca are coada un singur client inseamna
; ca ala seteaza et-ul, folosit la add-counter
(define (modify-et minutes C)
  (if (null? (cdr (get-queue C)))
      ((et+ minutes) C)
      C))

; pentru remove-first, cand face min-et sa ignore
; counterele cu coada goala
(define (min-et-no-zero counters) (min-elem counters get-et-no-zero))
(define (get-et-no-zero C)
  (if (null? (get-queue C))
      +inf.0
      (get-et C)))

(define (average-tt counters)
  (define sum-tt
    (apply + (map get-tt counters)))
  (/ sum-tt (length counters)))

; singura metoda pe care am gasit-o pentru
; a adauga la final
; append nu merge ca da un list*
(define (append-struct a b)
  (flatten (list a b)))

(define (serve requests fast-counters slow-counters)
  (define all-counters (append fast-counters slow-counters))
  (define (process-request condition function)
    (if (equal? condition #t)
        (serve (cdr requests)
               (function fast-counters) slow-counters)
        (serve (cdr requests)
               fast-counters (function slow-counters))))

  (define (process-request-min condition function min-function)
    (define min-slow (min-function slow-counters))
    (define min-fast (min-function fast-counters))
      ; am luat <= intre min-fast si min-slow pt ca cele cu index-ul mai
      ; mic au prioritate si toate din fast-counter au index-ul mai mic decat slow-counters        
      (if (and condition (<= (cdr min-fast) (cdr min-slow)))
          (serve (cdr requests)
                 (update function fast-counters (car min-fast))
                 slow-counters)
          (serve (cdr requests) fast-counters
                 (update function slow-counters (car min-slow)))))

  
  (define (ensure counters max-avg last-index)
    (define all-counters (append fast-counters counters))
    (if (> (average-tt all-counters) max-avg)
        (ensure (append-struct counters
                        (empty-counter (add1 last-index)))
                max-avg
                (add1 last-index))
        counters))
  
  (if (null? requests)
      all-counters
      (match (car requests)
        ; ensure trebuie sa fie inainte de add-to-counter pentru ca formatul se aseamana
        [(list 'ensure average) (serve (cdr requests) fast-counters (ensure slow-counters average (length all-counters)))]
        [(list 'delay index minutes)
         ; cresc tt-ul si et-ul cu delay-ul respectiv
         (process-request (<= index (get-index (last fast-counters)))
                          (update-curry (update-tt-et minutes minutes) index))]
        [(list name n-items)
         (define (add-to-counter-et C) (modify-et n-items ((add-to-counter name n-items) C)))                      
         (process-request-min (<= n-items ITEMS) add-to-counter-et min-tt)]
        [(list 'remove-first)
         (process-request-min #t remove-first-from-counter min-et-no-zero)])))
