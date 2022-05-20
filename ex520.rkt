;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex520) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination generates all states reachable with n boat trips before
; it looks at states that require n + 1 boat trips, hence terminates
; if the final state is reachable
 
(check-expect (solve initial-puzzle) final-puzzle)
 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))