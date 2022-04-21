;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex399) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick (non-same names (arrangements names))))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names

(check-expect (arrangements '("a" "b"))
              '(("a" "b")
                ("b" "a")))

(define (arrangements names)
  (cond
    [(empty? names) '(())]
    [else
     (insert-everywhere* (first names)
                         (arrangements (rest names)))]))

; String [List-of [List-of String]] -> [List-of [List-of String]]
; inserts x at the beginning, between all strings, and at
; the end of all items of l

(check-expect (insert-everywhere* "a" '(("b" "c")
                                        ("c" "b")))
              '(("a" "b" "c")
                ("b" "a" "c")
                ("b" "c" "a")
                ("a" "c" "b")
                ("c" "a" "b")
                ("c" "b" "a")))

(define (insert-everywhere* x l)
  (foldr append '()
         (map (lambda (v) (insert-everywhere x v)) l)))

; String [List-of String] -> [List-of [List-of String]]
; inserts x at the beginning, between all strings, and at
; the end of l

(check-expect (insert-everywhere "a" '()) '(("a")))
(check-expect (insert-everywhere "a" '("b" "c"))
              '(("a" "b" "c")
                ("b" "a" "c")
                ("b" "c" "a")))

(define (insert-everywhere x l)
  (cond
    [(empty? l) `((,x))]
    [else
     (cons (cons x l)
           (map (lambda (m) (cons (first l) m))
                (insert-everywhere x (rest l))))]))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (pick l (random (length l))))

; [NEList-of X] N -> X
; picks i'th number from l

(check-error (pick '() 0))
(check-error (pick '() 3))
(check-expect (pick '(1 2 3 4) 0) 1)
(check-expect (pick '(1 2 3 4) 2) 3)

(define (pick l i)
  (cond
    [(empty? l) (error "empty list")]
    [(zero? i) (first l)]
    [(positive? i) (pick (rest l) (sub1 i))]))
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place

(check-expect (non-same '("a" "b" "c")
                        '(("b" "c" "a")
                          ("b" "a" "c")))
              '(("b" "c" "a")))

(define (non-same names ll)
  (filter
   (lambda (l) (not (agrees-at-some-place? names l))) ll))

; [List-of String] [List-of String] -> Boolean
; #true if l1 agrees with l2 at any place; #false otherwise
; assume: input lists are of same length

(check-expect (agrees-at-some-place? '("a" "b" "c")
                                     '("b" "c" "a"))
              #false)
(check-expect (agrees-at-some-place? '("a" "b" "c")
                                     '("b" "a" "c"))
              #true)

(define (agrees-at-some-place? l1 l2)
  (cond
    [(empty? l1) #false]
    [(cons? l1)
     (or (string=? (first l1) (first l2))
         (agrees-at-some-place? (rest l1) (rest l2)))]))