;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex506) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X -> Y] [List-of X] -> [List-of Y]
; constructs a new list by applying a function to each item on l

(check-expect (map-acc-style add1 '(1 3 7 12 89 234))
              (map add1 '(1 3 7 12 89 234)))

(define (map-acc-style f l0)
  (local (; [List-of X] [List-of Y] -> [List-of Y]
          ; accumulator a is the list of f(x) for all x that l lacks
          ; from l0
          (define (map/a l a)
            (cond
              [(empty? l) (reverse a)]
              [else
               (map/a (rest l) (cons (f (first l)) a))])))
    (map/a l0 '())))