;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex521) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define RIVER (rectangle 384 256 'solid 'cornflowerblue))
(define SHORE (rectangle 384 64 'solid 'darkseagreen))
(define MISSIONARY (circle 8 'solid 'black))
(define CANNIBAL (overlay (circle 8 'outline 'black)
                          (circle 8 'solid 'white)))
(define BOAT (overlay (rectangle 36 20 'outline 'black)
                      (rectangle 36 20 'solid 'brown)))
(define SPACE (square 5 'solid 'transparent))

(define-struct state (ml cl mr cr boat))
(define LEFT 0)
(define RIGHT 1)
; A PuzzleState is a structure:
;   (make-state N N N N Side)
; where Side is one of:
; — LEFT
; — RIGHT

(define ex-init (make-state 3 3 0 0 LEFT))
(define ex-mid-1 (make-state 3 2 0 1 RIGHT))
(define ex-mid-2 (make-state 3 1 0 2 RIGHT))
(define ex-mid-3 (make-state 2 2 1 1 RIGHT))
(define ex-mid-4 (make-state 1 3 2 0 RIGHT))
(define ex-mid-5 (make-state 2 3 1 0 RIGHT))
(define ex-right-1 (make-state 2 3 1 0 LEFT))
(define ex-right-2 ex-init)
(define ex-right-3 (make-state 3 2 0 1 LEFT))
(define ex-final (make-state 0 0 3 3 RIGHT))

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination generates all states reachable with n boat trips before
; it looks at states that require n + 1 boat trips, hence terminates
; if the final state is reachable
 
(check-expect (solve ex-init) ex-final)
 
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

; PuzzleState -> Boolean
; are all people on the right river bank

(check-expect (final? ex-init) #false)
(check-expect (final? ex-mid-1) #false)
(check-expect (final? ex-right-1) #false)
(check-expect (final? ex-final) #true)

(define (final? s)
  (and (zero? (state-ml s))
       (zero? (state-cl s))))

; PuzzleState -> Image
; maps a state of the puzzle to an image

(check-expect (render-mc ex-mid-2)
              (above (render-side 3 1 #false)
                     RIVER
                     (render-side 0 2 #true)))

(define (render-mc s)
  (above (render-side (state-ml s)
                      (state-cl s)
                      (= (state-boat s) LEFT))
         RIVER
         (render-side (state-mr s)
                      (state-cr s)
                      (= (state-boat s) RIGHT))))

; N N Boolean -> Image
; renders the side

(check-expect (render-side 1 2 #true)
              (overlay (beside MISSIONARY SPACE CANNIBAL
                               SPACE CANNIBAL SPACE BOAT)
                       SHORE))

(define (render-side m c boat)
  (local ((define (render-people m c)
            (cond
              [(positive? m)
               (beside MISSIONARY SPACE (render-people (sub1 m) c))]
              [(positive? c)
               (beside CANNIBAL SPACE (render-people m (sub1 c)))]
              [else empty-image])))
    (overlay (beside (render-people m c) (if boat BOAT empty-image))
             SHORE)))