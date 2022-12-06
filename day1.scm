#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))


(define (split-by lst delim)
  ;; split a list at each occurance of delimiter, dropping the delimiter from the list
  (fold-right (lambda (element next)
                 (if (eqv? element delim)
                   (cons '() next)
                   (cons (cons element (car next)) (cdr next))))
              (list '()) lst))
              
(define (split-byf lst pred)
  ;; split a list at each item which fulfils a predicate, dropping the item from the list
  (fold-right (lambda (element next)
                 (if (pred element)
                   (cons '() next)
                   (cons (cons element (car next)) (cdr next))))
              (list '()) lst))
            
  
;; read the file
(define contents (call-with-input-file "inputs/day1_1.txt" get-string-all))

;; parse the file into a list of lists containing the "calories" of each elf as numbers
(define (strings->numbers strings) (map (lambda (str) (string->number str)) strings))
(define elf-lists (map strings->numbers
                   (split-byf
                     (string-split contents #\newline) 
                      string-null?)))

(define (max-element lst) (fold max 0 lst)) 

(define (sum lst) ( fold + 0 lst))

(define (largest-sum list-of-lists)
    (max-element (map sum list-of-lists)))

    
(define (take3 lst) (list (car lst) (cadr lst) (caddr lst)))

(sum (take3 (sort (elf-lists) >)))
