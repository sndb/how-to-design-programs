;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex352) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define x 5)
(define y 3)

(define expr-5
  'x)
(define expr-8
  (make-add 'x 3))
(define expr-7.5
  (make-mul 1/2 (make-mul 'x 3)))
(define expr-34
  (make-add (make-mul 'x 'x)
            (make-mul 'y 'y)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; replace all occurrences of x by v

(check-expect (subst expr-5 'x 3) 3)
(check-expect (subst expr-8 'x 10)
              (make-add 10 3))
(check-expect (subst expr-7.5 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst expr-34 'y 2)
              (make-add (make-mul 'x 'x)
                        (make-mul 2 2)))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))