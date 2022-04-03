;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FT -> ???
(define (fun-FT an-ftree)
  (cond
    [(no-parent? an-ftree) ...]
    [else (... (fun-FT (child-father an-ftree)) ...
               ... (fun-FT (child-mother an-ftree)) ...
               ... (child-name an-ftree) ...
               ... (child-date an-ftree) ...
               ... (child-eyes an-ftree) ...)]))

; FT N -> Number
; produces the average age of all child structures in an-ftree

(check-expect (average-age Carl 2000) 74)
(check-expect (average-age Gustav 2000)
              (/ (+ 12 34 35 74 74) 5))

(define (average-age an-ftree year)
  (local (; FT -> N
          (define (total-age an-ftree)
            (cond
              [(no-parent? an-ftree) 0]
              [else (+ (total-age (child-father an-ftree))
                       (total-age (child-mother an-ftree))
                       (- year (child-date an-ftree)))])))
    (/ (total-age an-ftree) (count-persons an-ftree))))

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