#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.

; in loc de o lista goala, dau o coada goala
(define (empty-counter index)
  (make-counter index 0 0 empty-queue))

(define (get-index C)
  (match C
    [(counter index tt et queue)
     index]))

; !nu interactioneaza cu coada, nu trb modificat
(define (update-curry f index)
  (lambda (counters)
      (map (lambda (x)
          (if (= (get-index x) index)
              (f x)
              x))
       counters)))

(define (update f counters index) ((update-curry f index) counters))

(define (get-tt C)
  (match C
    [(counter index tt et queue)
     tt]))

; !nu interactioneaza cu coada, nu trb modificat
(define (tt+ minutes)
    (lambda (C)
      (struct-copy counter C
               [tt (+ minutes
                      (get-tt C))])))

(define (get-et C)
  (match C
    [(counter index tt et queue)
     et]))

; !nu interactioneaza cu coada, nu trb modificat
(define (et+ minutes)
    (lambda (C)
      (struct-copy counter C
               [et (+ minutes
                      (get-et C))])))

; intoarce coada
(define (get-queue C)
  (match C
    [(counter index tt et queue)
     queue]))

; formatul de primire al parametrilor folosit de mine la etapa 2
; e acelasi cu cel testat de checker la etapa 3
; in loc de append pentru adaugare, folosesc enqueue
; la etapa asta pare ca e responsabilitatea add-to-counter-ului
; sa actualizeze et-ul

(define (add-to-counter name n-items)
  (lambda (C)
    (define (add-element-to-queue)
      ((tt+ n-items) (struct-copy counter C
                                  [queue (enqueue
                                          (cons name n-items)
                                          (get-queue C) )])))

    ; daca coada e vida inseamna ca trebuie sa-i actualizez
    ; et-ul la timpul de procesare al noului client
    (if (queue-empty? (get-queue C))
     ((et+ n-items) (add-element-to-queue))
     (add-element-to-queue))))

; Intoarce minimul dintre C1 si C2 conform definitiei
; !nu interactioneaza cu coada, nu trb modificat
(define (min-counter C1 C2 get-elem)
  (cond
    ((< (get-elem C1) (get-elem C2)) C1)
    ((> (get-elem C1) (get-elem C2)) C2)
    (else (if (< (get-index C1) (get-index C2))
              C1
              C2))))

; Intoarce structura de tip counter minima din lista counters
; !nu interactioneaza cu coada, nu trb modificat
(define (min-struct counters get-elem)
  (if (null? (cdr counters))
             (car counters)
             (min-counter (car counters) (min-struct (cdr counters) get-elem) get-elem)))

; Intoarce perechea de tip (index elem) aferenta minimului din lista counters
; !nu interactioneaza cu coada, nu trb modificat
(define (min-elem counters get-elem)
  (define result (min-struct counters get-elem))
  (cons (get-index result) (get-elem result)))

(define (min-tt counters) (min-elem counters get-tt))
(define (min-et counters) (min-elem counters get-et))

; intoarce o copie a structurii C primita ca parametru in care
; se adauga minutes-tt la tt si minutes-et la et
; !nu interactioneaza cu coada, nu trb modificat
(define (update-tt-et minutes-tt minutes-et)
  (lambda (C)
    ((et+ minutes-et) ((tt+ minutes-tt) C))))

; intoarce negatul unui numar primit ca parametru
(define (neg number)
  (- 0 number))

; elimina primul client, folosesc dequeue in loc de
; cdr
(define (eliminate-client C)
  (struct-copy counter C
                      [queue (dequeue (get-queue C))]))

(define (remove-first-from-counter C)
  ; daca n-are clienti, nu-i fac nimic
  ; daca ala era singurul client, atunci
  ; actualizez tt si et la 0 adaugand negatul respectiv
  ; altfel pentru tt sterg et-ul, iar pe et
  ; il resetez si adaug timpul de procesare a celui de-al
  ; doilea client
  (define q (get-queue C))
  (if (queue-empty? q)
      C
      ; queue-one-elem? este in queue.rkt
      (if (queue-one-elem? q)
          ((update-tt-et (neg (get-tt C)) (neg (get-et C))) (eliminate-client C))
          ((update-tt-et (neg (get-et C))
                         ; am inlocuit car cu top
                         (+ (neg (get-et C)) (cdr (top (get-queue (eliminate-client C))))))
           (eliminate-client C)))))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!

; scad pentru et si tt cu maximul dintre valoarea proprie si minutes
; pentru a ma asigura ca nu se va ajunge la timpi negativi
; Ex: daca tt = 5 si minutes = 8, atunci -5 > -8 si tt ajunge sa
; fie 0 in loc de -3
(define (pass-time-through-counter minutes)
  (λ (C)
    ((update-tt-et (max (neg minutes)
                        (neg (get-tt C)))
                   (max (neg minutes)
                        (neg (get-et C))))
                   C)))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (average-tt counters)
  (define sum-tt
    (apply + (map get-tt counters)))
  (/ sum-tt (length counters)))

; singura metoda pe care am gasit-o pentru
; a adauga la final
; append nu merge ca da un list*
(define (append-struct a b)
  (flatten (list a b)))

; scoate customerii din coada si scade timpul trecut
; din et si tt
; si face lista pentru left-customers
(define (pass-time-counter minutes)
  (lambda (C)
    (let iter ((minutes-left minutes) (new-C C) (left-customers '()))
      (define client (top (get-queue new-C)))
      ; daca nu mai sunt clienti sau timp sau exit-ul e prea mare intorc C-ul
      ; scazand timpul ramas (0 pentru zero? minutes-left = #t)
      (if (or (null? client) (> (get-et new-C) minutes-left))
          ; o intorc cu reverse pentru a pastra ordinea iesirii pentru cand va fi sortata lista
          (append (list (reverse left-customers)) ((pass-time-through-counter minutes-left) new-C))
          ; elimin primul client si continui cu timpul
          ; ramas la urmatoarea iteratie resetand et-ul si scazand din tt et-ul
          (iter (- minutes-left (get-et new-C))
                (remove-first-from-counter ((pass-time-through-counter (get-et new-C)) new-C))
                ; adaug si et-ul pentru a avea dupa ce sa ordonez lista
                (cons (cons (get-et C) (cons (get-index new-C) (car client)))
                      left-customers))))))

; inspirat din insert sort-ul prezentat la curs, modificat pentru problema curenta
(define (sort-waiting-list L)
    (if (null? L)
        L
        (insert (car L) (sort-waiting-list (cdr L)))))

; daca are et-ul mai mic il insereaza inainte
(define (insert x L)
  (if (or (null? L) (<= (car x) (car (car L))))
      (cons x L)
      (cons (car L) (insert x (cdr L)))))

(define (serve requests fast-counters slow-counters)
  (let serve->iter ((requests requests)
             (fast-counters fast-counters)
             (slow-counters slow-counters)
             (left-customers '()))
    
    (define all-counters (append fast-counters slow-counters))
    (define (process-request condition function)
      (if (equal? condition #t)
          (serve->iter (cdr requests)
                 (function fast-counters) slow-counters left-customers)
          (serve->iter (cdr requests)
                 fast-counters (function slow-counters) left-customers)))

    (define (process-request-min condition function min-function)
      (define min-slow (min-function slow-counters))
      (define min-fast (min-function fast-counters))
      ; am luat <= intre min-fast si min-slow pt ca cele cu index-ul mai
      ; mic au prioritate si toate din fast-counter au index-ul mai mic decat slow-counters        
      (if (and condition (<= (cdr min-fast) (cdr min-slow)))
          (serve->iter (cdr requests)
                 (update function fast-counters (car min-fast))
                 slow-counters
                 left-customers)
          (serve->iter (cdr requests) fast-counters
                 (update function slow-counters (car min-slow))
                 left-customers)))
  
    (define (ensure counters max-avg last-index)
      (define all-counters (append fast-counters counters))
      (if (> (average-tt all-counters) max-avg)
          (ensure (append-struct counters
                                 (empty-counter (add1 last-index)))
                  max-avg
                  (add1 last-index))
          counters))
    
    (if (null? requests)
        (append (list left-customers) all-counters)
        (match (car requests)
          ; ensure trebuie sa fie inainte de add-to-counter pentru ca formatul se aseamana
          [(list 'ensure average) (serve->iter (cdr requests)
                                               fast-counters
                                               (ensure slow-counters average (length all-counters))
                                               left-customers)]
          [(list 'delay index minutes)
           ; cresc tt-ul si et-ul cu delay-ul respectiv
           (process-request (<= index (get-index (last fast-counters)))
                            (update-curry (update-tt-et minutes minutes) index))]
          [(list name n-items)
           (define (add-to-counter-et C) ((add-to-counter name n-items) C))                      
           (process-request-min (<= n-items ITEMS) add-to-counter-et min-tt)]
          [time
           (define fast-counters-pairs (map (pass-time-counter time) fast-counters))
           (define slow-counters-pairs (map (pass-time-counter time) slow-counters))
           (serve->iter (cdr requests)
                        ; pentru counterele scap de listele de astepare aplicand cdr
                        (map cdr fast-counters-pairs)
                        (map cdr slow-counters-pairs)
                        ; pentru lista de asteptare scap de countere aplicand car
                        ; apoi aplic append pe lista cu listele  de forma(et counter-id client)
                        ; apoi sortez lista dupa et si aplic cdr ca sa scap de et
                        (append left-customers
                                (map cdr
                                     (sort-waiting-list
                                          (apply append (map car
                                                             (append fast-counters-pairs slow-counters-pairs)))))))
                        ]))))
        
