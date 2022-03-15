;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex273) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx

(check-expect (map-from-fold sqr '(1 2 3))
              '(1 4 9))

(define (map-from-fold f lx)
  (local (; X [List-of Y] -> [List-of Y]
          (define (g x y)
            (cons (f x) y)))
    (foldr g '() lx)))