;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex507) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X Y -> Y] Y [List-of X] -> Y

(check-expect (f*ldl + 0 '(1 2 3))
              (foldl + 0 '(1 2 3)))
(check-expect (f*ldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
 
(define (f*ldl f e l0)
  (local (; Y [List-of X] -> Y
          ; accumulator a is the result of applying f(x, a) for all x
          ; that l lacks from l0
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    (fold/a e l0)))

(check-expect (f*ldl.v2 + 0 '(1 2 3))
              (foldl + 0 '(1 2 3)))
(check-expect (f*ldl.v2 cons '() '(a b c))
              (foldl cons '() '(a b c)))

(define (f*ldl.v2 f i l)
  (cond
    [(empty? l) i]
    [else (f*ldl.v2 f (f (first l) i) (rest l))]))

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to the numbers
; between 0 and (- n0 1)

(check-expect (build-l*st 5 add1) (build-list 5 add1))
(check-expect (build-l*st 10 sub1) (build-list 10 sub1))

(define (build-l*st n0 f)
  (local (; N [List-of X] -> [List-of X]
          ; accumulator a is the result of applying f(n) for all n
          ; in the interval [n, n0)
          (define (build-l*st/a n a)
            (cond
              [(zero? n) a]
              [else
               (build-l*st/a (sub1 n) (cons (f (sub1 n)) a))])))
    (build-l*st/a n0 '())))