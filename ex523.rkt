;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex523) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define RIVER (rectangle 384 256 'solid 'cornflowerblue))
(define SHORE (rectangle 384 64 'solid 'darkseagreen))
(define MISSIONARY (circle 8 'solid 'black))
(define CANNIBAL (overlay (circle 8 'outline 'black)
                          (circle 8 'solid 'white)))
(define BOAT (overlay (rectangle 36 20 'outline 'black)
                      (rectangle 36 20 'solid 'brown)))
(define SPACE (square 5 'solid 'transparent))

(define-struct state (ml cl mr cr boat prev))
(define LEFT 0)
(define RIGHT 1)
; A PuzzleState is a structure:
;   (make-state N N N N Side [List-of PuzzleState])
; where Side is one of:
; — LEFT
; — RIGHT
; accumulator prev is a list of states traversed to get there

(define ex-init (make-state 3 3 0 0 LEFT '()))
(define ex-mid-1 (make-state 3 2 0 1 RIGHT `(,ex-init)))
(define ex-mid-2 (make-state 3 1 0 2 RIGHT `(,ex-init)))
(define ex-mid-3 (make-state 2 2 1 1 RIGHT `(,ex-init)))
(define ex-mid-4 (make-state 1 3 2 0 RIGHT `(,ex-init)))
(define ex-mid-5 (make-state 2 3 1 0 RIGHT `(,ex-init)))
(define ex-right-1 (make-state 2 3 1 0 LEFT `(,ex-mid-3 ,ex-init)))
(define ex-right-2 (make-state 3 3 0 0 LEFT `(,ex-mid-3 ,ex-init)))
(define ex-right-3 (make-state 3 2 0 1 LEFT `(,ex-mid-3 ,ex-init)))
(define ex-final (make-state 0 0 3 3 RIGHT '()))

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination generates all states reachable with n boat trips before
; it looks at states that require n + 1 boat trips, hence terminates
; if the final state is reachable
 
(check-satisfied (solve ex-init) final?)
 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else
               (solve* (filter valid-state?
                               (create-next-states los)))])))
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

; [List-of PuzzleState] -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach

(check-satisfied (create-next-states (list ex-init))
                 (lambda (ss)
                   (and (member? ex-mid-1 ss)
                        (member? ex-mid-2 ss)
                        (member? ex-mid-3 ss)
                        (member? ex-mid-4 ss)
                        (member? ex-mid-5 ss))))

(define (create-next-states ss)
  (cond
    [(empty? ss) '()]
    [else
     (append (create-next-states/state (first ss))
             (create-next-states (rest ss)))]))

; PuzzleState -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach
(define (create-next-states/state s)
  (local ((define ml (state-ml s))
          (define cl (state-cl s))
          (define mr (state-mr s))
          (define cr (state-cr s))
          (define boat (state-boat s))
          (define prev (cons s (state-prev s))))
    (cond
      [(= LEFT boat)
       (list
        (make-state (- ml 1) cl (+ mr 1) cr RIGHT prev)
        (make-state (- ml 2) cl (+ mr 2) cr RIGHT prev)
        (make-state ml (- cl 1) mr (+ cr 1) RIGHT prev)
        (make-state ml (- cl 2) mr (+ cr 2) RIGHT prev)
        (make-state (- ml 1) (- cl 1) (+ mr 1) (+ cr 1)
                    RIGHT prev))]
      [(= RIGHT boat)
       (list
        (make-state (+ ml 1) cl (- mr 1) cr LEFT prev)
        (make-state (+ ml 2) cl (- mr 2) cr LEFT prev)
        (make-state ml (+ cl 1) mr (- cr 1) LEFT prev)
        (make-state ml (+ cl 2) mr (- cr 2) LEFT prev)
        (make-state (+ ml 1) (+ cl 1) (- mr 1) (- cr 1)
                    LEFT prev))])))

; PuzzleState -> Boolean
; is s valid

(check-expect (valid-state? ex-init) #true)
(check-expect (valid-state? ex-mid-1) #true)
(check-expect (valid-state? ex-mid-2) #true)
(check-expect (valid-state? ex-mid-3) #true)
(check-expect (valid-state? ex-mid-4) #false)
(check-expect (valid-state? ex-mid-5) #false)
(check-expect (valid-state? ex-right-1) #false)
(check-expect (valid-state? ex-right-2) #false)
(check-expect (valid-state? ex-right-3) #true)
(check-expect (valid-state? ex-final) #true)

(define (valid-state? s)
  (local ((define ml (state-ml s))
          (define cl (state-cl s))
          (define mr (state-mr s))
          (define cr (state-cr s))
          (define boat (state-boat s)))
    (and (andmap (lambda (x) (>= x 0)) (list ml cl mr cr))
         (or (zero? ml) (<= cl ml))
         (or (zero? mr) (<= cr mr))
         (not (encountered? s)))))

; PuzzleState -> Boolean
; was s encountered previously

(check-expect (encountered? ex-init) #false)
(check-expect (encountered? ex-mid-1) #false)
(check-expect (encountered? ex-mid-2) #false)
(check-expect (encountered? ex-mid-3) #false)
(check-expect (encountered? ex-mid-4) #false)
(check-expect (encountered? ex-mid-5) #false)
(check-expect (encountered? ex-right-1) #false)
(check-expect (encountered? ex-right-2) #true)
(check-expect (encountered? ex-right-3) #false)
(check-expect (encountered? ex-final) #false)

(define (encountered? s)
  (local (; PuzzleState [List-of PuzzleState] -> Boolean
          (define (encountered-helper l)
            (cond
              [(empty? l) #false]
              [else
               (local ((define p (first l)))
                 (or (and (= (state-ml s) (state-ml p))
                          (= (state-cl s) (state-cl p))
                          (= (state-mr s) (state-mr p))
                          (= (state-cr s) (state-cr p))
                          (= (state-boat s) (state-boat p)))
                     (encountered-helper (rest l))))])))
    (encountered-helper (state-prev s))))