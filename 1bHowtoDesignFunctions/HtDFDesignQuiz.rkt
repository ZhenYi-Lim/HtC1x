;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HtDFDesignQuiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image -> Image -> Boolean
;; checks if the first image is larger (area = width*height) than the second.
(check-expect (imageLarger? (square 1 "solid" "red") (square 2 "solid" "red")) false)
(check-expect (imageLarger? (square 2 "solid" "red") (square 1 "solid" "red")) true)
(check-expect (imageLarger? (square 2 "solid" "red") (square 2 "solid" "red")) false)

;(define (imageLarger? img1 img2) false) ;stub

;(define (imageLarger? img1 img2) ;template
;  (... img1 img2))

(define (imageLarger? img1 img2)
  (> (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))