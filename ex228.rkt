;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex228) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; FSM-State FSM-State -> Boolean
; are s1 and s2 equal?

(check-expect (state=? "red" "orange") #false)
(check-expect (state=? "red" "red") #true)

(define (state=? s1 s2)
  (string=? s1 s2))

(define-struct fs [fsm current])
; A SimulationState is a structure:
;   (make-fs FSM FSM-State)

; SimulationState -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState KeyEvent -> SimulationState
; finds the next state from ke and cs

(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))

(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black")
             "not found: black")

(define (find transitions current)
  (cond
    [(empty? transitions) (error "not found: " current)]
    [else
     (if (current? current (first transitions))
         (transition-next (first transitions))
         (find (rest transitions) current))]))

; FSM-State Transition -> Boolean
; is s equals to the current field of t?

(check-expect (current? "red" (make-transition "red" "green")) #true)
(check-expect (current? "red" (make-transition "yellow" "red")) #false)

(define (current? s t)
  (state=? s (transition-current t)))

; FSM FSM-State -> SimulationState
; match the keys pressed with the given FSM
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))