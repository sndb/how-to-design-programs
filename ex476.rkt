;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex476) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string

(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "a") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)

(define (fsm-match? an-fsm a-string)
  (local ((define initial (fsm-initial an-fsm))
          (define transitions (fsm-transitions an-fsm))
          (define final (fsm-final an-fsm))
          
          ; FSM-State [List-of 1String] -> Boolean
          (define (fsm-match-helper? current keys)
            (cond
              [(empty? keys) (string=? current final)]
              [else
               (local ((define next
                         (next-state transitions current (first keys))))
                 (cond
                   [(boolean? next) #false]
                   [else (fsm-match-helper? next (rest keys))]))])))
    (fsm-match-helper? initial (explode a-string))))

; [List-of 1Transition] FSM-State 1String -> [Maybe FSM-State]
; gets a next state; #false otherwise

(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "AA" "a") "BC")
(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "AA" "d") #false)
(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "BC" "b") "BC")
(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "BC" "c") "BC")
(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "BC" "d") "DD")
(check-expect
 (next-state (fsm-transitions fsm-a-bc*-d) "DD" "d") #false)

(define (next-state transitions current key)
  (cond
    [(empty? transitions) #false]
    [else
     (local ((define transition (first transitions)))
       (if (and (string=? current (transition-current transition))
                (string=? key (transition-key transition)))
           (transition-next transition)
           (next-state (rest transitions) current key)))]))