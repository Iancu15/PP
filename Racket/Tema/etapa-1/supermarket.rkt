#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (get-tt C)
  (match C
    [(counter index tt queue)
     tt]))

(define (tt+ C minutes)
  (struct-copy counter C
               [tt (+ minutes
                      (get-tt C))]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (get-index C)
  (match C
    [(counter index tt queue)
     index]))

; Intoarce minimul dintre C1 si C2 conform definitiei
(define (min-counter C1 C2)
  (cond
    ((< (get-tt C1) (get-tt C2)) C1)
    ((> (get-tt C1) (get-tt C2)) C2)
    (else (if (< (get-index C1) (get-index C2))
              C1
              C2))))

; Intoarce structura de tip counter minima din lista counters
(define (min-tt-struct counters)
  (if (null? (cdr counters))
             (car counters)
             (min-counter (car counters) (min-tt-struct (cdr counters)))))

; Intoarce perechea de tip (index tt) aferenta minimului din lista counters
(define (min-tt counters)
  (match (min-tt-struct counters)
    [(counter index tt queue)
     (cons index tt)]))

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (get-queue C)
  (match C
    [(counter index tt queue)
     queue]))

(define (add-to-counter C name n-items)
  (tt+ (struct-copy counter C
               [queue (append (get-queue C) (list (cons name n-items)))])
       n-items))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește

; padding pentru a avea o semnatura cu elemente cat add-to-counter
(define (tt+->serve C minutes padding)
  (tt+ C minutes))

; daca numarul de produse sunt mai multe decat numarul maxim pentru prima
; casa o exclud si aplic min-tt doar pe restul de case din counters
(define (min-tt->serve counters n-items)
  (if (> n-items ITEMS)
      (min-tt (cdr counters))
      (min-tt counters)))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  ; aplica request-ul si apeleaza serve pentru a trece prin urmatoarele request-uri
  (define (modify-counter index function command1 command2 requests)
    (define (modify C) (function C command1 command2))
    (cond
      ((= index 1) (serve requests (modify C1) C2 C3 C4))
      ((= index 2) (serve requests C1 (modify C2) C3 C4))
      ((= index 3) (serve requests C1 C2 (modify C3) C4))
      ((= index 4) (serve requests C1 C2 C3 (modify C4)))
      ))

  (define (counter-list) (list C1 C2 C3 C4))

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (modify-counter index tt+->serve minutes 0 (cdr requests))]
        [(list name n-items)         (modify-counter (car (min-tt->serve (counter-list) n-items)) add-to-counter name n-items (cdr requests))])))
