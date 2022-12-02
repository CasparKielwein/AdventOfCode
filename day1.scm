#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1))

(define (max-element lst) (fold max 0 lst))
    
(define (sum lst) ( fold + 0 lst))

(define (largest-sum list-of-lists)
    (max-element (map sum list-of-lists)))
