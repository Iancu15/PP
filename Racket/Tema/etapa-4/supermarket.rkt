#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue availability) #:transparent)

; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

; in loc de o lista goala, dau o coada goala
(define (empty-counter index)
  (make-counter index 0 0 empty-queue #t))

(define (get-index C)
  (match C
    [(counter index tt et queue availability)
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
    [(counter index tt et queue availability)
     tt]))

; !nu interactioneaza cu coada, nu trb modificat
(define (tt+ minutes)
    (lambda (C)
      (struct-copy counter C
               [tt (+ minutes
                      (get-tt C))])))

(define (get-et C)
  (match C
    [(counter index tt et queue availability)
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
    [(counter index tt et queue availability)
     queue]))

(define (get-availability C)
  (match C
    [(counter index tt et queue availability)
     availability]))

(define (add-to-counter name n-items)
  (lambda (C)
    (define (add-element-to-queue)
      ((tt+ n-items) (struct-copy counter C
                                  [queue (enqueue
                                          (cons name n-items)
                                          (get-queue C) )])))

    ; daca coada e vida inseamna ca trebuie sa-i actualizez
    ; et-ul la timpul de procesare al noului client
    ;!! in cazul in care nu e valabil nu se adauga clientul
    ;!! caz necesar pentru momentul in care toate casele sunt inchise
    ;(if (equal? (get-availability C) #t)
        (if (queue-empty? (get-queue C))
            ((et+ n-items) (add-element-to-queue))
            (add-element-to-queue))))
        ;C)))

; Intoarce minimul dintre C1 si C2 conform definitiei
; !nu interactioneaza cu coada, nu trb modificat
;!! pentru ca atunci cand cauda pentru a adauga la tt-ul
;!! minim sa prioritizeze casele deschise
(define (min-counter C1 C2 get-elem)
  (cond
    ((equal? (get-availability C1) #f) C2)
    ((equal? (get-availability C2) #f) C1)
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

(define (pass-time-through-counter minutes)
  (λ (C)
    ((update-tt-et (max (neg minutes)
                        (neg (get-tt C)))
                   (max (neg minutes)
                        (neg (get-et C))))
                   C)))
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (available? C)
  (get-availability C))

;!! elimin cu filter casele inchise
(define (average-tt counters)
  (define open-counters (filter available? counters))
  (define sum-tt
    (apply + (map get-tt open-counters)))
  (/ sum-tt (length open-counters)))

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
                ;!! in cazul in care ies mai multi simultan, dar aveau exit time-urile diferiti
                ;!! trebuie sa-i aduc la acelasi nivel si anume scad minutele
                (cons (cons (- (get-et C) minutes-left) (cons (get-index new-C) (car client)))
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

; elimina casele goale
(define (remove-clear-counters counters)
  (if (null? counters)
      '()
      (if (queue-empty? (get-queue (car counters)))
          (remove-clear-counters (cdr counters))
          (cons (car counters) (remove-clear-counters (cdr counters))))))

; inchide o casa
(define close-counter
    (lambda (C)
      (struct-copy counter C
               [availability #f])))

(define (find-counter counters index)
  (if (null? counters)
      '()
      (if (= (get-index (car counters)) index)
          (car counters)
          (find-counter (cdr counters) index))))

(define (serve requests fast-counters slow-counters)
  (let serve->iter ((requests requests)
             (fast-counters fast-counters)
             (slow-counters slow-counters)
             (left-customers '()))
;    (display fast-counters)
;    (display slow-counters)
;    (display "\n")
;    (display left-customers)
;    (display "\n\n")
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
      ; !! conform enuntul de la etapa 4, casa trebuie sa fie deschisa pentru a adauga
      ; !! si noi folosim process-request-min doar pentru a cauta la ce casa sa adaugam
      (if (and condition (<= (cdr min-fast) (cdr min-slow))
               (available? (find-counter fast-counters (car min-fast))))
          (serve->iter (cdr requests)
                 (update function fast-counters (car min-fast))
                 slow-counters
                 left-customers)
          (serve->iter (cdr requests) fast-counters
                 (update function slow-counters (car min-slow))
                 left-customers)))
  
    (define (ensure counters max-avg last-index)
      ; fast-counters nu se modifica
      (define all-counters (append fast-counters counters))
      (if (> (average-tt all-counters) max-avg)
          (ensure (append-struct counters
                                 (empty-counter (add1 last-index)))
                  max-avg
                  (add1 last-index))
          counters))

    ; transform lista de counters in combinatie de (index coada)
    ; si ordonez dupa index
    (define (process-counter-list counter-list)
      (sort-waiting-list (map (lambda (C)
                                (cons (get-index C) (get-queue C)))
                              counter-list)))
    
    (if (null? requests)
        (append (list left-customers) (process-counter-list (remove-clear-counters all-counters)))
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
          [(list 'close index)
           (process-request (<= index (get-index (last fast-counters)))
                            (update-curry close-counter index))]
          [(list name n-items)
           (define (add-counter C) ((add-to-counter name n-items) C))
           ; adauga in fast-counters daca respecta conditia (n-items <= ITEMS)
           ; !! SI daca cel putin o clasa din fast-counters e deschisa
           ; !! Testez asta transformand counterele in valoare de adevar a disponibilitatii
           ; !! lor si fac or intre toate aceste valor, daca toate sunt false inseamna
           ; !! ca nu pot alege fast-counters
           (process-request-min (<= n-items ITEMS) add-counter min-tt)]
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