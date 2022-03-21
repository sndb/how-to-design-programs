;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname isl-for-loops-examples-reformulated) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [X -> Y] [List-of X] -> [Maybe Y]
; collects the values of all iterations with and;
; returns the last acceptable value or #false

(check-expect (and-map (lambda (i) (> (- 9 i) 0))
                       (build-list 10 identity))
              #false)
(check-expect (and-map (lambda (i) (if (>= i 0) i #false))
                       (build-list 10 identity))
              9)

(define (and-map f l)
  (foldl (lambda (x y)
           (if (and (not (false? y))
                    (not (false? (f x))))
               x
               #false))
         #true
         l))

; [X -> Y] [List-of X] -> [Maybe Y]
; collects the values of all iterations with or;
; returns the first value that is not #false

(check-expect (or-map (lambda (i) (if (= (- 9 i) 0) i #false))
                      (build-list 10 identity))
              9)
(check-expect (or-map (lambda (i) (if (< i 0) i #false))
                      (build-list 10 identity))
              #false)

(define (or-map f l)
  (foldl (lambda (x y)
           (if (or (not (false? y))
                   (not (false? (f x))))
               x
               #false))
         #false
         l))

(check-expect
 (for/and ([i 10]) (> (- 9 i) 0))
 (and-map (lambda (i) (> (- 9 i) 0))
          (build-list 10 identity)))

(check-expect
 (for/and ([i 10]) (if (>= i 0) i #false))
 (and-map (lambda (i) (if (>= i 0) i #false))
          (build-list 10 identity)))

(check-expect
 (for/or ([i 10]) (if (= (- 9 i) 0) i #false))
 (or-map (lambda (i) (if (= (- 9 i) 0) i #false))
         (build-list 10 identity)))

(check-expect
 (for/or ([i 10]) (if (< i 0) i #false))
 (or-map (lambda (i) (if (< i 0) i #false))
         (build-list 10 identity)))

(check-expect
 (for/sum ([c "abc"]) (string->int c))
 (foldr (lambda (c y) (+ y (string->int c)))
        0
        '("a" "b" "c")))

(check-expect
 (for/product ([c "abc"]) (+ (string->int c) 1))
 (foldr (lambda (c y) (* y (+ (string->int c) 1)))
        1
        '("a" "b" "c")))

(define a (string->int "a"))
(check-expect
 (for/string ([j 10]) (int->string (+ a j)))
 (foldl (lambda (j y) (string-append y (int->string (+ a j))))
        ""
        (build-list 10 identity)))