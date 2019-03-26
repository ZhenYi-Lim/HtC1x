;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-strings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants:
(define S1 "")
(define S2 "hello")
(define S3 "zebra")
(define S4 "melon")

(define TEXT-SIZE 20)
(define TEXT-COLOUR "black")

(define BLANK (square 0 "solid" "white"))

;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons S2 (cons S3 empty)))
(define LOS3 (cons S3 (cons S2 empty)))
(define LOS4 (cons S2 (cons S4 (cons S3 empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-string (first los))
              (fn-for-los (rest los)))]))

;; Functions:

;; ListOfString -> Image
;; sort strings alphabetically and lay them out vertically
(check-expect (arrange-strings (cons S2 (cons S1 empty)))
        (above/align "left"
                     (text S1 TEXT-SIZE TEXT-COLOUR)
                     (text S2 TEXT-SIZE TEXT-COLOUR)))
(check-expect (arrange-strings (cons S1 (cons S2 empty)))
        (above/align "left"
                     (text S1 TEXT-SIZE TEXT-COLOUR)
                     (text S2 TEXT-SIZE TEXT-COLOUR)))

(define (arrange-strings los)
  (layout-strings (sort-strings los)))

;; ListOfString -> Image
;; place images above each other in order of list
(check-expect (layout-strings empty) BLANK)
(check-expect (layout-strings (cons S1 (cons S2 empty)))
              (above/align "left"
                          (text S1 TEXT-SIZE TEXT-COLOUR)
                          (text S2 TEXT-SIZE TEXT-COLOUR)
                          BLANK))

;(define (layout-strings los) BLANK)  ;stub

(define (layout-strings los)
  (cond [(empty? los) BLANK]
        [else
         (above/align "left"
                      (text (first los) TEXT-SIZE TEXT-COLOUR)
                      (layout-strings (rest los)))]))

;; ListOfString -> ListOfString
;; sort strings into alphabetical order
(check-expect (sort-strings empty) empty)
(check-expect (sort-strings LOS2) LOS2)
(check-expect (sort-strings LOS3) LOS2)
;(define (sort-strings los) los) ;stub
(define (sort-strings los)
  (cond [(empty? los) empty]
        [else
         (insert-string (first los) (sort-strings (rest los)))]))

;; String ListOfString -> ListOfString
;; insert string into ListOfString in alphabetical order
;; ASSUME: ListOfString is already sorted.
(check-expect (insert-string S4 empty) (cons S4 empty))
(check-expect (insert-string S4 LOS2) LOS4) 
;(define (insert-string "Apple" LOS2) LOS2) ;stub
(define (insert-string s los)
  (cond [(empty? los) (cons s los)]
        [else
         (if (string>=? s (first los))
             (cons (first los) (insert-string s (rest los)))
             (cons s los))]))