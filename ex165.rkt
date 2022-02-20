;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex165) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of:
; – '()
; – (cons String List-of-numbers)

; List-of-strings -> List-of-strings
; consumes a list of toy descriptions (one-word strings)
; and replaces all occurrences of "robot" with "r2d2"

(check-expect (subst-robot '()) '())
(check-expect
 (subst-robot (cons "hello" (cons "robot" '())))
 (cons "hello" (cons "r2d2" '())))

(define (subst-robot l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (if (string=? "robot" (first l))
          "r2d2"
          (first l))
      (subst-robot (rest l)))]))

; String String List-of-strings -> List-of-strings
; produces a new list of strings by substituting all occurrences
; of old with new

(check-expect (substitute "r2d2" "robot" '()) '())
(check-expect
 (substitute "r2d2" "robot" (cons "hello" (cons "robot" '())))
 (cons "hello" (cons "r2d2" '())))

(define (substitute new old l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (if (string=? old (first l))
          new
          (first l))
      (substitute new old (rest l)))]))