;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ListMechanism) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

empty

(define L1 (cons "Planes" empty))      ; a list of 1 element
;(cons "Leafs" (cons "Flames" empty)) ; a list of 2 elements
(define L2 (cons 10 (cons 9 (cons 10 empty))))
(define L3 (cons (square 10 "solid" "blue")
      (cons (triangle 20 "solid" "green")
            empty)))

(first L1)
(first l2)
(first L3)

(rest L1)
(rest L2)
(rest L3)

(rest (rest L2))
(first (rest (rest L2)))

(empty? empty)
(empty? L1)
(empty? 1) ;false