;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex491) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

; [List-of X] -> [List-of X]
; reverses l

(check-expect (my-reverse '()) '())
(check-expect (my-reverse '(1 2 3)) '(3 2 1))
(check-expect (my-reverse (build-list 10000 add1))
              (reverse (build-list 10000 add1)))

(define (my-reverse l)
  (cond
    [(empty? l) '()]
    [else
     (append (my-reverse (rest l)) (list (first l)))]))