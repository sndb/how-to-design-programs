;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex230) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; using SimulationState to track the current state

(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
(define-struct ss [fsm current])
; An FSM is a structure:
;   (make-fsm FSM-State LOT FSM-State)
; An FSM-State is a Color.
; A LOT is one of:
;   – '()
;   – (cons Transition LOT)
; A Transition is a structure:
;   (make-transition FSM-State KeyEvent FSM-State)
; A SimulationState is a structure:
;   (make-ss FSM FSM-State)

(define lot-109
  (list (make-transition "white" "a" "yellow")
        (make-transition "yellow" "b" "yellow")
        (make-transition "yellow" "c" "yellow")
        (make-transition "yellow" "d" "green")))

(define fsm-109
  (make-fsm "white"
            lot-109
            "green"))

; FSM-State FSM-State -> Boolean
; are s1 and s2 equal?

(check-expect (state=? "red" "orange") #false)
(check-expect (state=? "red" "red") #true)

(define (state=? s1 s2)
  (string=? s1 s2))

; SimulationState -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square
               (make-ss fsm-109 "yellow"))
              (square 100 "solid" "yellow"))

(define (state-as-colored-square s)
  (colored-square (ss-current s)))

; FSM-State -> Image
; renders s as a colored square

(check-expect (colored-square "green")
              (square 100 "solid" "green"))

(define (colored-square s)
  (square 100 "solid" s))

; SimulationState KeyEvent -> SimulationState
; finds the next state from ke and s

(check-expect
 (find-next-state (make-ss fsm-109 "white") "a")
 (make-ss fsm-109 "yellow"))
(check-expect
 (find-next-state (make-ss fsm-109 "yellow") "c")
 (make-ss fsm-109 "yellow"))
(check-expect
 (find-next-state (make-ss fsm-109 "yellow") "d")
 (make-ss fsm-109 "green"))

(define (find-next-state s ke)
  (make-ss (ss-fsm s)
           (find (ss-fsm s) (ss-current s) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in an-fsm
; and retrieves the next field

(check-expect (find fsm-109 "white" "a") "yellow")
(check-expect (find fsm-109 "yellow" "b") "yellow")
(check-expect (find fsm-109 "yellow" "d") "green")
(check-error (find fsm-109 "black" "x") "not found: black/x")

(define (find an-fsm current ke)
  (find/lot (fsm-transitions an-fsm) current ke))

; LOT FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find/lot lot-109 "white" "a") "yellow")
(check-expect (find/lot lot-109 "yellow" "b") "yellow")
(check-expect (find/lot lot-109 "yellow" "d") "green")
(check-error (find/lot lot-109 "black" "x") "not found: black/x")

(define (find/lot transitions current ke)
  (cond
    [(empty? transitions)
     (error "not found: " current "/" ke)]
    [else
     (if (represents? current ke (first transitions))
         (transition-next (first transitions))
         (find/lot (rest transitions) current ke))]))

; FSM-State KeyEvent Transition -> Boolean
; is t represents s and ke?

(check-expect
 (represents? "white"
              "a"
              (make-transition "white" "a" "yellow"))
 #true)
(check-expect
 (represents? "white"
              "b"
              (make-transition "white" "a" "yellow"))
 #false)
(check-expect
 (represents? "yellow"
              "b"
              (make-transition "yellow" "b" "yellow"))
 #true)
(check-expect
 (represents? "yellow"
              "d"
              (make-transition "yellow" "d" "green"))
 #true)

(define (represents? s ke t)
  (and (state=? s (transition-current t))
       (key=? ke (transition-key t))))

; SimulationState -> Boolean
; is final state reached?

(check-expect
 (final-state-reached? (make-ss fsm-109 "white"))
 #false)
(check-expect
 (final-state-reached? (make-ss fsm-109 "yellow"))
 #false)
(check-expect
 (final-state-reached? (make-ss fsm-109 "green"))
 #true)

(define (final-state-reached? s)
  (final-state? (ss-fsm s) (ss-current s)))

; FSM FSM-State -> Boolean
; is s the final state of an-fsm?

(check-expect (final-state? fsm-109 "white") #false)
(check-expect (final-state? fsm-109 "yellow") #false)
(check-expect (final-state? fsm-109 "green") #true)

(define (final-state? an-fsm s)
  (state=? (fsm-final an-fsm) s))

; FSM -> SimulationState
; starts the simulation
(define (fsm-simulate an-fsm)
  (big-bang (make-ss an-fsm (fsm-initial an-fsm))
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state-reached? state-as-colored-square]))