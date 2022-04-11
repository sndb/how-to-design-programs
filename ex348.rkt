;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex348) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct and-expr [left right])
; An And is a structure:
;   (make-and-expr Boolean-expr Boolean-expr)
; interpretation: (make-and-expr #true #false) represents an
; expression (and #true #false)

(define-struct or-expr [left right])
; An Or is a structure:
;   (make-or-expr Boolean-expr Boolean-expr)
; interpretation: (make-or-expr #true #false) represents an
; expression (or #true #false)

(define-struct not-expr [arg])
; A Not is a structure:
;   (make-not-expr Boolean-expr)
; interpretation: (make-not-expr #true) represents an
; expression (not #true)

; A Boolean-expr is one of:
; — And
; — Or
; — Not
; — Boolean

; A Boolean is one of:
; — #true
; — #false

(define expr-false #false)
(define expr-and-true
  (make-and-expr (make-or-expr #true #false)
                 #true))
(define expr-or-false
  (make-or-expr #false
                (make-and-expr #true #false)))
(define expr-not-true
  (make-not-expr #false))

; Boolean-expr -> Boolean
; computes the value of expr

(check-expect (eval-bool-expression expr-false) #false)
(check-expect (eval-bool-expression expr-and-true) #true)
(check-expect (eval-bool-expression expr-or-false) #false)
(check-expect (eval-bool-expression expr-not-true) #true)

(define (eval-bool-expression e)
  (cond
    [(and-expr? e)
     (and (eval-bool-expression (and-expr-left e))
          (eval-bool-expression (and-expr-right e)))]
    [(or-expr? e)
     (or (eval-bool-expression (or-expr-left e))
         (eval-bool-expression (or-expr-right e)))]
    [(not-expr? e)
     (not (eval-bool-expression (not-expr-arg e)))]
    [else e]))