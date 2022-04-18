;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex380) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons (cons FSM-State (cons KeyEvent '()))
;         (cons FSM-State '()))
; An FSM-State is a String that specifies a color
 
; data examples 
(define fsm-traffic
  '((("red" "a") "green")
    (("green" "b") "yellow")
    (("yellow" "c") "red")))
 
; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay (text current 16 'black)
                (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions `(,current ,key-event)))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist

(check-expect (find fsm-traffic '("red" "a")) "green")
(check-expect (find fsm-traffic '("green" "b")) "yellow")
(check-expect (find fsm-traffic '("yellow" "c")) "red")
(check-error (find fsm-traffic '("yellow" "d")) "not found")
(check-error (find fsm-traffic "orange") "not found")

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))