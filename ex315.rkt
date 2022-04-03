;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex315) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; An FF (short for family forest) is a [List-of FT].
; interpretation: a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; [List-of FT] N -> Number
; produces the average age of all child instances a-forest

(check-expect (average-age ff1 2000) (/ (+ 74 74) 2))
(check-expect (average-age ff2 1990) (/ (+ 24 25 64 64) 4))
(check-expect (average-age ff3 1990) (/ (+ 24 25 64 64 64) 5))

(define (average-age a-forest year)
  (/ (foldr + 0
            (map (lambda (an-ftree) (total-age an-ftree year))
                 a-forest))
     (foldr + 0
            (map count-persons a-forest))))

; FT -> N
; counts the total age of the child structures in the tree

(check-expect (total-age Carl 2000) 74)
(check-expect (total-age Eva 1990) (+ 25 64 64))

(define (total-age an-ftree year)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (total-age (child-father an-ftree) year)
             (total-age (child-mother an-ftree) year)
             (- year (child-date an-ftree)))]))

; FT -> N
; counts the child structures in the tree

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (count-persons (child-father an-ftree))
             (count-persons (child-mother an-ftree))
             1)]))