#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define contents (call-with-input-file "inputs/day2.txt" get-string-all))

(define rounds (map (lambda (str) (string-split str #\space)) 
              (drop-right ;; drop the last line, the trailing newline
                (string-split contents #\newline)
                1)))

(define (points char)
  (cond
    ((equal? char #\A) 1)
    ((equal? char #\B) 2)
    ((equal? char #\C) 3)
    ((equal? char #\X) 1)
    ((equal? char #\Y) 2)
    ((equal? char #\Z) 3)))
   
(define (string->point str) (points (string-ref str 0)))
(define (list->match lst) (map string->point lst))
   
(define (raw-points lst) 
  (map 
    (lambda (round-strings) 
      (points ;; get the point value
        (string-ref (cadr round-strings) 0))) ;;from the second string in each round
    lst))

(define (result match)
  (cond
    ((equal? match '("A" "X")) 3)
    ((equal? match '("A" "Y")) 6)
    ((equal? match '("A" "Z")) 0)
    
    ((equal? match '("B" "X")) 0)
    ((equal? match '("B" "Y")) 3)
    ((equal? match '("B" "Z")) 6)
    
    ((equal? match '("C" "X")) 6)
    ((equal? match '("C" "Y")) 0)
    ((equal? match '("C" "Z")) 3)))


(+
  (sum (raw-points rounds))
  (sum (map result rounds)))
