;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation: An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

; FSM-State FSM-State -> Boolean
; are s1 and s2 equal?

(check-expect (state=? "red" "orange") #false)
(check-expect (state=? "red" "red") #true)

(define (state=? s1 s2)
  (string=? s1 s2))