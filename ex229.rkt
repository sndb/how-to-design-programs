;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex229) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct ktransition [current key next])
; A Transition is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; FSM-State is a Color.

; interpretation: An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define 109-machine
  (list (make-ktransition "white" "a" "yellow")
        (make-ktransition "yellow" "b" "yellow")
        (make-ktransition "yellow" "c" "yellow")
        (make-ktransition "yellow" "d" "green")))

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
               (make-fs 109-machine "yellow"))
              (square 100 "solid" "yellow"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState KeyEvent -> SimulationState
; finds the next state from ke and cs

(check-expect
 (find-next-state (make-fs 109-machine "white") "a")
 (make-fs 109-machine "yellow"))
(check-expect
 (find-next-state (make-fs 109-machine "yellow") "c")
 (make-fs 109-machine "yellow"))
(check-expect
 (find-next-state (make-fs 109-machine "yellow") "d")
 (make-fs 109-machine "green"))

(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find 109-machine "white" "a") "yellow")
(check-expect (find 109-machine "yellow" "b") "yellow")
(check-expect (find 109-machine "yellow" "d") "green")
(check-error (find 109-machine "black" "x")
             "not found: black/x")

(define (find transitions current ke)
  (cond
    [(empty? transitions) (error "not found: " current "/" ke)]
    [else
     (if (current? current ke (first transitions))
         (ktransition-next (first transitions))
         (find (rest transitions) current ke))]))

; FSM-State KeyEvent Transition -> Boolean
; are both s and ke equals to the corresponding fields of t?

(check-expect (current? "white"
                        "a"
                        (make-ktransition "white" "a" "yellow"))
              #true)
(check-expect (current? "white"
                        "b"
                        (make-ktransition "white" "a" "yellow"))
              #false)
(check-expect (current? "yellow"
                        "b"
                        (make-ktransition "yellow" "b" "yellow"))
              #true)
(check-expect (current? "yellow"
                        "d"
                        (make-ktransition "yellow" "d" "green"))
              #true)

(define (current? s ke t)
  (and (state=? s (ktransition-current t))
       (key=? ke (ktransition-key t))))

; FSM FSM-State -> SimulationState
; match the keys pressed with the given FSM
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))