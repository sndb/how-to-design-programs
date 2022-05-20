;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex517) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 '((λ (x) x) (λ (x) x)))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
(define ex7 'x)

; Lam -> Boolean

(check-expect (is-var? ex1) #false)
(check-expect (is-var? ex2) #false)
(check-expect (is-var? ex3) #false)
(check-expect (is-var? ex4) #false)
(check-expect (is-var? ex5) #false)
(check-expect (is-var? ex6) #false)
(check-expect (is-var? ex7) #true)

(define (is-var? le)
  (symbol? le))

; Lam -> Boolean

(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex2) #true)
(check-expect (is-λ? ex3) #true)
(check-expect (is-λ? ex4) #false)
(check-expect (is-λ? ex5) #false)
(check-expect (is-λ? ex6) #false)
(check-expect (is-λ? ex7) #false)

(define (is-λ? le)
  (and (list? le)
       (= 3 (length le))
       (symbol=? 'λ (first le))
       (list? (second le))
       (= 1 (length (second le)))
       (is-var? (first (second le)))
       (is-lam? (third le))))

; Lam -> Boolean

(check-expect (is-app? ex1) #false)
(check-expect (is-app? ex2) #false)
(check-expect (is-app? ex3) #false)
(check-expect (is-app? ex4) #true)
(check-expect (is-app? ex5) #true)
(check-expect (is-app? ex6) #true)
(check-expect (is-app? ex7) #false)

(define (is-app? le)
  (and (list? le)
       (= 2 (length le))
       (is-lam? (first le))
       (is-lam? (second le))))

; Lam -> Boolean

(check-expect (is-lam? ex1) #true)
(check-expect (is-lam? ex2) #true)
(check-expect (is-lam? ex3) #true)
(check-expect (is-lam? ex4) #true)
(check-expect (is-lam? ex5) #true)
(check-expect (is-lam? ex6) #true)
(check-expect (is-lam? ex7) #true)
(check-expect (is-lam? '(hello world)) #true)
(check-expect (is-lam? 1234) #false)
(check-expect (is-lam? '("hello" "world")) #false)

(define (is-lam? le)
  (or (is-var? le)
      (is-λ? le)
      (is-app? le)))

; Lam -> Symbol

(check-expect (λ-para ex1) 'x)
(check-expect (λ-para ex2) 'x)
(check-expect (λ-para ex3) 'y)

(define (λ-para le)
  (first (second le)))

; Lam -> Lam

(check-expect (λ-body ex1) 'x)
(check-expect (λ-body ex2) 'y)
(check-expect (λ-body ex3) '(λ (x) y))

(define (λ-body le)
  (third le))

; Lam -> Lam

(check-expect (app-fun ex4) '(λ (x) (x x)))
(check-expect (app-fun ex5) '(λ (x) x))
(check-expect (app-fun ex6) '((λ (y) (λ (x) y)) (λ (z) z)))

(define (app-fun le)
  (first le))

; Lam -> Lam

(check-expect (app-arg ex4) '(λ (x) (x x)))
(check-expect (app-arg ex5) '(λ (x) x))
(check-expect (app-arg ex6) '(λ (w) w))

(define (app-arg le)
  (second le))

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
 
(check-expect (undeclareds ex1)
              '(λ (x) (*declared x)))
(check-expect (undeclareds ex2)
              '(λ (x) (*undeclared y)))
(check-expect (undeclareds ex3)
              '(λ (y) (λ (x) (*declared y))))
(check-expect (undeclareds ex4)
              '((λ (x) ((*declared x) (*declared x)))
                (λ (x) ((*declared x) (*declared x)))))
(check-expect (undeclareds '((λ (x) x) (λ (y) x)))
              '((λ (x) (*declared x)) (λ (y) (*undeclared x))))
(check-expect (undeclareds '(λ (*undeclared)
                              ((λ (x) (x *undeclared))
                               y)))
              '(λ (*undeclared)
                 ((λ (x) ((*declared x) (*declared *undeclared)))
                  (*undeclared y))))

(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds)
                   `(*declared ,le)
                   `(*undeclared ,le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Lam -> Lam
; replaces all occurrences of variables with a natural number that
; represents how far away the declaring λ is

(check-expect (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))

(define (static-distance le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (static-distance/a le declareds)
            (cond
              [(is-var? le)
               (index declareds le)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para) (static-distance/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (static-distance/a fun declareds)
                       (static-distance/a arg declareds)))])))
    (static-distance/a le0 '())))

; [X] [List-of X] X -> [Maybe N]
; determines the index of x in l

(check-expect (index '(a bc def gh i) 'ij) #false)
(check-expect (index '(a bc def gh i) 'gh) 3)

(define (index l x)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? x (first l))
         0
         (local ((define maybe (index (rest l) x)))
           (if (boolean? maybe) #false (add1 maybe))))]))