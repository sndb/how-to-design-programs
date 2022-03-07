;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex230-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; using the initial field of FSM to track the current state

(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM is a structure:
;   (make-fsm FSM-State LOT FSM-State)
; An FSM-State is a Color.
; A LOT is one of:
;   – '()
;   – (cons Transition LOT)
; A Transition is a structure:
;   (make-transition FSM-State KeyEvent FSM-State)

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

; FSM -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square fsm-109)
              (square 100 "solid" "white"))

(define (state-as-colored-square an-fsm)
  (colored-square (fsm-initial an-fsm)))

; FSM-State -> Image
; renders s as a colored square

(check-expect (colored-square "green")
              (square 100 "solid" "green"))

(define (colored-square s)
  (square 100 "solid" s))

; FSM FSM-State -> FSM
; changes the initial state to s
(define (set-state an-fsm s)
  (make-fsm s (fsm-transitions an-fsm) (fsm-final an-fsm)))

; FSM KeyEvent -> FSM
; finds the next state from ke and an-fsm

(check-expect
 (find-next-state (set-state fsm-109 "white") "a")
 (set-state fsm-109 "yellow"))
(check-expect
 (find-next-state (set-state fsm-109 "yellow") "c")
 (set-state fsm-109 "yellow"))
(check-expect
 (find-next-state (set-state fsm-109 "yellow") "d")
 (set-state fsm-109 "green"))

(define (find-next-state an-fsm ke)
  (set-state an-fsm (find an-fsm ke)))

; FSM KeyEvent -> FSM-State
; finds the state representing initial in an-fsm
; and retrieves the next field

(check-expect (find (set-state fsm-109 "white") "a") "yellow")
(check-expect (find (set-state fsm-109 "yellow") "b") "yellow")
(check-expect (find (set-state fsm-109 "yellow") "d") "green")
(check-error (find (set-state  fsm-109 "black") "x")
             "not found: black/x")

(define (find an-fsm ke)
  (find/lot (fsm-transitions an-fsm)
            (fsm-initial an-fsm)
            ke))

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

; FSM -> Boolean
; is final state reached?

(check-expect
 (final-state-reached? (set-state fsm-109 "white"))
 #false)
(check-expect
 (final-state-reached? (set-state fsm-109 "yellow"))
 #false)
(check-expect
 (final-state-reached? (set-state fsm-109 "green"))
 #true)

(define (final-state-reached? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

; FSM -> SimulationState
; starts the simulation
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state-reached? state-as-colored-square]))
