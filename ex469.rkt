;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex469) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define N ; a TM
  '((2 2 3 10)
    (3 9 21)
    (1 2)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix

(check-expect (lhs (first M)) '(2 2 3))

(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix

(check-expect (rhs (first M)) 10)

(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
; verifies the Solution s to the SOE m

(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 2 2)) #false)

(define (check-solution m s)
  (local (; Equation -> Boolean
          (define (check-equation e)
            (= (plug-in (lhs e) s) (rhs e))))
    (andmap check-equation m)))

; [List-of Number] Solution -> Number
(define (plug-in l s)
  (foldr (lambda (i j a) (+ (* i j) a)) 0 l s))

; Equation Equation -> Equation
; subtracts a multiple of e2 from e1

(check-expect (subtract (second M) (first M)) '(3 9 21))
(check-expect (subtract (third M) (first M)) '(-3 -8 -19))

(define (subtract e1 e2)
  (local ((define m (/ (first e1) (first e2))))
    (rest (map - e1 (map (lambda (x) (* x m)) e2)))))

; SOE -> TM
; triangulates the given system of equations

(check-expect (triangulate '((3 9 21)
                             (-3 -8 -19)))
              '((3 9 21)
                (1 2)))
(check-expect (triangulate M) N)
(check-expect (triangulate '((2 3 3 8)
                             (2 3 -2 3)
                             (4 -2 2 4)))
              '((2 3 3 8)
                (-8 -4 -12)
                (-5 -5)))
(check-error (triangulate '((2 2 2 6)
                            (2 2 4 8)
                            (2 2 1 2)))
             "encountered an SOE whose leading coefficients are all 0")

(define (triangulate m)
  (cond
    [(andmap (lambda (e) (= 0 (first e))) m)
     (error "encountered an SOE whose leading coefficients are all 0")]
    [(= 0 (first (first m)))
     (triangulate (append (rest m) (list (first m))))]
    [(= (length m) 1) m]
    [else
     (cons (first m)
           (triangulate (map (lambda (e) (subtract e (first m)))
                             (rest m))))]))

; TM -> Solution
; solves m

(check-satisfied (solve N) (lambda (r) (check-solution M r)))

(define (solve m)
  (local (; Equation Solution -> Solution
          ; solves a single linear equation in n+1 variables, given a
          ; solution for the last n variables
          (define (solve-equation e s)
            (cond
              [(empty? s) (list (/ (rhs e) (first e)))]
              [else
               (local ((define result (plug-in (rest (lhs e)) s))
                       (define answer (/ (- (rhs e) result) (first e))))
                 (cons answer s))])))
    (foldr solve-equation '() m)))