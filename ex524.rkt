;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex524) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RIVER (rectangle 256 384 'solid 'cornflowerblue))
(define SHORE (rectangle 64 384 'solid 'darkseagreen))
(define SPACE (square 5 'solid 'transparent))
(define MISSIONARY (circle 8 'solid 'black))
(define CANNIBAL (overlay (circle 8 'outline 'black)
                          (circle 8 'solid 'white)))
(define BOAT (overlay (rectangle 20 36 'outline 'black)
                      (rectangle 20 36 'solid 'brown)))

(define-struct ps (ml cl mr cr boat prev))
; A PuzzleState is a structure:
;   (make-state N N N N Side [List-of PuzzleState])
; where Side is one of:
; — 'left
; — 'right
; accumulator prev is a list of states traversed to get there

(define ex-init (make-ps 3 3 0 0 'left '()))
(define ex-mid-1 (make-ps 3 2 0 1 'right `(,ex-init)))
(define ex-mid-2 (make-ps 3 1 0 2 'right `(,ex-init)))
(define ex-mid-3 (make-ps 2 2 1 1 'right `(,ex-init)))
(define ex-mid-4 (make-ps 1 3 2 0 'right `(,ex-init)))
(define ex-mid-5 (make-ps 2 3 1 0 'right `(,ex-init)))
(define ex-right-1 (make-ps 2 3 1 0 'left `(,ex-mid-3 ,ex-init)))
(define ex-right-2 (make-ps 3 3 0 0 'left `(,ex-mid-3 ,ex-init)))
(define ex-right-3 (make-ps 3 2 0 1 'left `(,ex-mid-3 ,ex-init)))
(define ex-final (make-ps 0 0 3 3 'right '()))

; Number -> [List-of Image]
; plays the movie, spending n seconds per image
(define (play-movie n)
  (run-movie n (map render-mc (solve ex-init))))

; PuzzleState -> [List-of PuzzleState]
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination generates all states reachable with n boat trips before
; it looks at states that require n + 1 boat trips, hence terminates
; if the final state is reachable
 
(check-satisfied (solve ex-init)
                 (lambda (ss)
                   (and (equal? ex-init (first ss))
                        (final? (first (reverse ss))))))
 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (local ((define result (first (filter final? los))))
                 (reverse (cons result (ps-prev result))))]
              [else
               (solve* (filter valid? (create-next-states los)))])))
    (solve* (list state0))))

; PuzzleState -> Boolean
; are all people on the right river bank

(check-expect (final? ex-init) #false)
(check-expect (final? ex-mid-1) #false)
(check-expect (final? ex-right-1) #false)
(check-expect (final? ex-final) #true)

(define (final? s)
  (and (zero? (ps-ml s)) (zero? (ps-cl s))))

; PuzzleState -> Image
; maps a state of the puzzle to an image

(check-expect
 (render-mc ex-mid-2)
 (beside (overlay (above MISSIONARY SPACE MISSIONARY SPACE MISSIONARY
                         SPACE CANNIBAL SPACE) SHORE)
         RIVER
         (overlay (above CANNIBAL SPACE CANNIBAL SPACE BOAT) SHORE)))

(define (render-mc s)
  (local (; N N -> Image
          (define (render-people m c)
            (cond
              [(positive? m)
               (above MISSIONARY SPACE (render-people (sub1 m) c))]
              [(positive? c)
               (above CANNIBAL SPACE (render-people m (sub1 c)))]
              [else empty-image]))

          ; N N Boolean -> Image
          (define (render-side m c b)
            (overlay (above (render-people m c)
                            (if b BOAT empty-image))
                     SHORE))

          ; Image
          (define left-side
            (render-side (ps-ml s) (ps-cl s)
                         (symbol=? (ps-boat s) 'left)))

          ; Image
          (define right-side
            (render-side (ps-mr s) (ps-cr s)
                         (symbol=? (ps-boat s) 'right))))
    (beside left-side RIVER right-side)))

; [List-of PuzzleState] -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach

(check-satisfied (create-next-states (list ex-init))
                 (lambda (ss) (and (member? ex-mid-1 ss)
                                   (member? ex-mid-2 ss)
                                   (member? ex-mid-3 ss)
                                   (member? ex-mid-4 ss)
                                   (member? ex-mid-5 ss))))

(define (create-next-states ss)
  (cond
    [(empty? ss) '()]
    [else
     (local ((define s (first ss)))
       (append (list (move-people s 2 0)
                     (move-people s 1 0)
                     (move-people s 1 1)
                     (move-people s 0 1)
                     (move-people s 0 2))
               (create-next-states (rest ss))))]))

; PuzzleState N N -> PuzzleState
; moves m missionaries and c cannibals to the other side

(check-expect (move-people ex-init 0 1) ex-mid-1)
(check-expect (move-people ex-init 0 2)  ex-mid-2)
(check-expect (move-people ex-init 1 1) ex-mid-3)
(check-expect (move-people ex-mid-3 0 1) ex-right-1)
(check-expect (move-people ex-mid-3 1 1) ex-right-2)

(define (move-people s m c)
  (if (symbol=? 'left (ps-boat s))
      (make-ps (- (ps-ml s) m) (- (ps-cl s) c)
               (+ (ps-mr s) m) (+ (ps-cr s) c)
               'right (cons s (ps-prev s)))
      (make-ps (+ (ps-ml s) m) (+ (ps-cl s) c)
               (- (ps-mr s) m) (- (ps-cr s) c)
               'left (cons s (ps-prev s)))))

; PuzzleState -> Boolean
; is s valid

(check-expect (valid? ex-init) #true)
(check-expect (valid? ex-mid-1) #true)
(check-expect (valid? ex-mid-2) #true)
(check-expect (valid? ex-mid-3) #true)
(check-expect (valid? ex-mid-4) #false)
(check-expect (valid? ex-mid-5) #false)
(check-expect (valid? ex-right-1) #false)
(check-expect (valid? ex-right-2) #false)
(check-expect (valid? ex-right-3) #true)
(check-expect (valid? ex-final) #true)

(define (valid? s)
  (local ((define ml (ps-ml s))
          (define cl (ps-cl s))
          (define mr (ps-mr s))
          (define cr (ps-cr s)))
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
  (ormap (lambda (t) (ps=? s t)) (ps-prev s)))

; PuzzleState PuzzleState -> Boolean
; are s1 and s2 equal
(define (ps=? s1 s2)
  (and (= (ps-ml s1) (ps-ml s2))
       (= (ps-cl s1) (ps-cl s2))
       (= (ps-mr s1) (ps-mr s2))
       (= (ps-cr s1) (ps-cr s2))
       (symbol=? (ps-boat s1) (ps-boat s2))))