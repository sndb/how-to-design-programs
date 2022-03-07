;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex219) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SEGMENT-SIZE 20)
(define SEGMENT-RADIUS (/ SEGMENT-SIZE 2))
(define SEGMENT-COLOR "red")
(define FOOD-COLOR "green")
(define VSEGMENTS 24)
(define HSEGMENTS 48)
(define VCENTER (/ VSEGMENTS 2))
(define HCENTER (/ HSEGMENTS 2))

(define SEGMENT
  (circle SEGMENT-RADIUS "solid" SEGMENT-COLOR))
(define FOOD
  (circle SEGMENT-RADIUS "solid" FOOD-COLOR))
(define SCENE
  (empty-scene (* HSEGMENTS SEGMENT-SIZE)
               (* VSEGMENTS SEGMENT-SIZE)))

; A Segment is a Posn.
; interpretation: (make-posn x y) represents a segment
; at the position (x,y)
(define ex-segment0 (make-posn 0 0))
(define ex-segment1 (make-posn HCENTER VCENTER))

(define UP 0)
(define DOWN 1)
(define RIGHT 2)
(define LEFT 3)
; A Direction is one of:
; — UP
; — DOWN
; — RIGHT
; — LEFT

; A Tail is one of:
; — '()
; — (cons Segment Tail)
; interpretation: a sequence of connected segments
(define ex-tail (list (make-posn 0 0)
                      (make-posn 0 1)
                      (make-posn 0 2)))

(define-struct worm [tail direction])
; A Worm is a structure:
;   (make-worm Tail Direction)
; interpretation: (make-worm t d) represents a worm with
; tail t moving in the direction d
(define ex-worm (make-worm ex-tail RIGHT))

; A Food is a Posn.
; interpretation: (make-posn x y) represents a piece of
; food located at the position (x,y)
(define ex-food (make-posn 24 32))

(define-struct game [worm food])
; A Game is a structure:
;   (make-game Worm Food)
; interpretation: (make-worm w f) represents a worm w
; and its food f
(define ex-game (make-game ex-worm ex-food))

; Worm -> Worm
; moves w 1 segment forward in its direction

(check-expect
 (move-worm
  (make-worm (list (make-posn 0 0) (make-posn 1 0))
             RIGHT))
 (make-worm (list (make-posn 1 0) (make-posn 2 0))
            RIGHT))

(check-expect
 (move-worm
  (make-worm (list (make-posn 10 10) (make-posn 10 11))
             LEFT))
 (make-worm (list (make-posn 10 11) (make-posn 9 11))
            LEFT))

(check-expect
 (move-worm
  (make-worm (list (make-posn 0 0) (make-posn 1 0))
             DOWN))
 (make-worm (list (make-posn 1 0) (make-posn 1 1))
            DOWN))

(check-expect
 (move-worm
  (make-worm (list (make-posn 5 5) (make-posn 6 5))
             UP))
 (make-worm (list (make-posn 6 5) (make-posn 6 4))
            UP))

(define (move-worm w)
  (make-worm (move-tail (worm-tail w) (worm-direction w))
             (worm-direction w)))

; Game -> Game
; updates the game state, moves the worm and grows
; its tail on eating food

(check-expect
 (update-game
  (make-game (make-worm (list (make-posn 5 5)
                              (make-posn 6 5)
                              (make-posn 7 5))
                        RIGHT)
             (make-posn 9 5)))
 (make-game (make-worm (list (make-posn 6 5)
                             (make-posn 7 5)
                             (make-posn 8 5))
                       RIGHT)
            (make-posn 9 5)))

(check-random
 (update-game
  (make-game (make-worm (list (make-posn 6 5)
                              (make-posn 7 5)
                              (make-posn 8 5))
                        RIGHT)
             (make-posn 9 5)))
 (make-game (make-worm (list (make-posn 6 5)
                             (make-posn 7 5)
                             (make-posn 8 5)
                             (make-posn 9 5))
                       RIGHT)
            (food-create (make-posn 9 5))))

(define (update-game g)
  (if (will-eat-food? (game-worm g) (game-food g))
      (make-game (eat-food (game-worm g) (game-food g))
                 (food-create (game-food g)))
      (make-game (move-worm (game-worm g))
                 (game-food g))))

; Worm Food -> Boolean
; would the worm eat food on the next tick?

(check-expect
 (will-eat-food? (make-worm (list (make-posn 5 5)
                                  (make-posn 6 5)
                                  (make-posn 7 5))
                            RIGHT)
                 (make-posn 9 5))
 #false)

(check-expect
 (will-eat-food? (make-worm (list (make-posn 6 5)
                                  (make-posn 7 5)
                                  (make-posn 8 5))
                            RIGHT)
                 (make-posn 9 5))
 #true)

(define (will-eat-food? w f)
  (member? f (worm-tail (move-worm w))))

; Worm Food -> Worm
; adds f to the worm's tail

(check-expect
 (eat-food (make-worm (list (make-posn 6 5)
                            (make-posn 7 5)
                            (make-posn 8 5))
                      RIGHT)
           (make-posn 9 5))
 (make-worm (list (make-posn 6 5)
                  (make-posn 7 5)
                  (make-posn 8 5)
                  (make-posn 9 5))
            RIGHT))

(define (eat-food w f)
  (make-worm (append (worm-tail w) (list f))
             (worm-direction w)))

; Tail Direction -> Tail
; moves t in the direction d

(check-expect (move-tail '() UP) '())

(check-expect (move-tail (list (make-posn 0 0)
                               (make-posn 1 0))
                         RIGHT)
              (list (make-posn 1 0)
                    (make-posn 2 0)))

(check-expect (move-tail (list (make-posn 10 10)
                               (make-posn 10 11))
                         LEFT)
              (list (make-posn 10 11)
                    (make-posn 9 11)))

(check-expect (move-tail (list (make-posn 0 0)
                               (make-posn 1 0))
                         DOWN)
              (list (make-posn 1 0)
                    (make-posn 1 1)))

(check-expect (move-tail (list (make-posn 5 5)
                               (make-posn 6 5))
                         UP)
              (list (make-posn 6 5)
                    (make-posn 6 4)))

(define (move-tail t d)
  (cond
    [(empty? t) '()]
    [(cons? t)
     (append (rest t)
             (list (move-segment (head t) d)))]))

; Segment Direction -> Segment
; moves s according to d

(check-expect (move-segment (make-posn 0 0) RIGHT)
              (make-posn 1 0))
(check-expect (move-segment (make-posn 1 0) LEFT)
              (make-posn 0 0))
(check-expect (move-segment (make-posn 0 0) DOWN)
              (make-posn 0 1))
(check-expect (move-segment (make-posn 0 1) UP)
              (make-posn 0 0))

(define (move-segment s d)
  (cond
    [(= d UP) (make-posn (posn-x s) (sub1 (posn-y s)))]
    [(= d DOWN) (make-posn (posn-x s) (add1 (posn-y s)))]
    [(= d RIGHT) (make-posn (add1 (posn-x s)) (posn-y s))]
    [(= d LEFT) (make-posn (sub1 (posn-x s)) (posn-y s))]))

; Tail -> Segment
; produces the head segment (last in the list)

(check-expect (head (list (make-posn 0 0)
                          (make-posn 0 1)
                          (make-posn 0 2)))
              (make-posn 0 2))

(define (head t)
  (first (reverse t)))

; Tail -> Tail
; produces the body segments (any but last in the list)

(check-expect (body (list (make-posn 0 0)
                          (make-posn 0 1)
                          (make-posn 0 2)))
              (list (make-posn 0 0)
                    (make-posn 0 1)))

(define (body t)
  (reverse (rest (reverse t))))

; Worm KeyEvent -> Worm
; changes the direction of w when arrow key is pressed

(check-expect
 (control-worm (make-worm (list (make-posn 0 0)) RIGHT)
               "a")
 (make-worm (list (make-posn 0 0)) RIGHT))

(check-expect
 (control-worm (make-worm (list (make-posn 0 0)) RIGHT)
               "right")
 (make-worm (list (make-posn 0 0)) RIGHT))

(check-expect
 (control-worm (make-worm (list (make-posn 0 0)) RIGHT)
               "left")
 (make-worm (list (make-posn 0 0)) LEFT))

(check-expect
 (control-worm (make-worm (list (make-posn 0 0)) RIGHT)
               "up")
 (make-worm (list (make-posn 0 0)) UP))

(check-expect
 (control-worm (make-worm (list (make-posn 0 0)) RIGHT)
               "down")
 (make-worm (list (make-posn 0 0)) DOWN))

(define (control-worm w ke)
  (cond
    [(key=? "up" ke) (make-worm (worm-tail w) UP)]
    [(key=? "down" ke) (make-worm (worm-tail w) DOWN)]
    [(key=? "right" ke) (make-worm (worm-tail w) RIGHT)]
    [(key=? "left" ke) (make-worm (worm-tail w) LEFT)]
    [else w]))

; Game KeyEvent -> Game
; controls the game using control-worm
(define (control-game g ke)
  (make-game (control-worm (game-worm g) ke) (game-food g)))

; Game -> Image
; renders the game g

(check-expect
 (render-game
  (make-game
   (make-worm (list (make-posn 8 8)
                    (make-posn 8 9)
                    (make-posn 8 10))
              DOWN)
   (make-posn 8 12)))
 (add-food-to-image
  (make-posn 8 12)
  (add-worm-to-image
   (make-worm (list (make-posn 8 8)
                    (make-posn 8 9)
                    (make-posn 8 10))
              DOWN)
   SCENE)))

(define (render-game g)
  (add-food-to-image
   (game-food g) (add-worm-to-image
                  (game-worm g) SCENE)))

; Food Image -> Image
; adds f to img

(check-expect
 (add-food-to-image (make-posn 8 12) SCENE)
 (place-image FOOD (sg->px 8) (sg->px 12) SCENE))

(define (add-food-to-image f img)
  (place-image FOOD (sg->px (posn-x f)) (sg->px (posn-y f)) img))

; Worm Image -> Image
; adds w to img

(check-expect
 (add-worm-to-image (make-worm (list (make-posn 0 0)
                                     (make-posn 0 1)
                                     (make-posn 0 2))
                               RIGHT)
                    SCENE)
 (add-tail-to-image (list (make-posn 0 0)
                          (make-posn 0 1)
                          (make-posn 0 2))
                    SCENE))

(define (add-worm-to-image w img)
  (add-tail-to-image (worm-tail w) img))

; Tail Image -> Image
; adds t to img

(check-expect
 (add-tail-to-image (list (make-posn 0 0)
                          (make-posn 0 1)
                          (make-posn 0 2))
                    SCENE)
 (place-image
  SEGMENT (sg->px 0) (sg->px 0)
  (place-image
   SEGMENT (sg->px 0) (sg->px 1)
   (place-image
    SEGMENT (sg->px 0) (sg->px 2)
    SCENE))))

(define (add-tail-to-image t img)
  (cond
    [(empty? t) img]
    [(cons? t)
     (add-segment-to-image (first t)
                           (add-tail-to-image (rest t) img))]))

; Segment Image -> Image
; adds s to img

(check-expect
 (add-segment-to-image (make-posn HCENTER VCENTER) SCENE)
 (place-image SEGMENT (sg->px HCENTER) (sg->px VCENTER) SCENE))

(define (add-segment-to-image s img)
  (place-image SEGMENT
               (sg->px (posn-x s))
               (sg->px (posn-y s))
               img))

; Number -> Number
; translates segment position to pixel offset

(check-expect (sg->px 0) (/ SEGMENT-SIZE 2))
(check-expect (sg->px 5) (+ (* 5 SEGMENT-SIZE) (/ SEGMENT-SIZE 2)))

(define (sg->px x)
  (+ (* x SEGMENT-SIZE) (/ SEGMENT-SIZE 2)))

; Worm -> Boolean
; is w has reached the walls of the world?

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn 0 0)) UP))
 #false)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn HCENTER VCENTER)) UP))
 #false)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn HCENTER VSEGMENTS)) UP))
 #true)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn HSEGMENTS VCENTER)) UP))
 #true)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn HSEGMENTS VSEGMENTS)) UP))
 #true)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn 1 -1))
             UP))
 #true)

(check-expect
 (run-into-walls?
  (make-worm (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn 2 0))
             RIGHT))
 #false)

(define (run-into-walls? w)
  (run-into-walls?/segment (head (worm-tail w))))

; Segment -> Boolean
; is s has reached the walls of the world?

(check-expect
 (run-into-walls?/segment (make-posn 0 0))
 #false)
(check-expect
 (run-into-walls?/segment (make-posn HCENTER VCENTER))
 #false)
(check-expect
 (run-into-walls?/segment (make-posn HCENTER VSEGMENTS))
 #true)
(check-expect
 (run-into-walls?/segment (make-posn HSEGMENTS VCENTER))
 #true)
(check-expect
 (run-into-walls?/segment (make-posn HSEGMENTS VSEGMENTS))
 #true)

(define (run-into-walls?/segment s)
  (or (not (and (<= 0 (posn-x s)) (< (posn-x s) HSEGMENTS)))
      (not (and (<= 0 (posn-y s)) (< (posn-y s) VSEGMENTS)))))

; Worm -> Boolean
; is w has ran into itself?

(check-expect
 (run-into-itself?
  (make-worm (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn 1 -1))
             UP))
 #false)

(check-expect
 (run-into-itself?
  (make-worm (list (make-posn 0 0)
                   (make-posn 0 1)
                   (make-posn 1 1)
                   (make-posn 1 0)
                   (make-posn 0 0))
             LEFT))
 #true)

(define (run-into-itself? w)
  (member? (head (worm-tail w)) (body (worm-tail w))))

; Game -> Image
; renders the final scene

(check-expect
 (final-scene
  (make-game
   (make-worm (list (make-posn 0 0)
                    (make-posn 1 0)
                    (make-posn 1 -1))
              UP)
   (make-posn HCENTER VCENTER)))
 (overlay/align
  "left" "bottom"
  (text "the worm hit the wall" SEGMENT-SIZE "black")
  (render-game
   (make-game
    (make-worm (list (make-posn 0 0)
                     (make-posn 1 0)
                     (make-posn 1 -1))
               UP)
    (make-posn HCENTER VCENTER)))))

(check-expect
 (final-scene
  (make-game
   (make-worm (list (make-posn 0 0)
                    (make-posn 0 1)
                    (make-posn 1 1)
                    (make-posn 1 0)
                    (make-posn 0 0))
              LEFT)
   (make-posn HCENTER VCENTER)))
 (overlay/align
  "left" "bottom"
  (text "the worm ran into itself" SEGMENT-SIZE "black")
  (render-game
   (make-game
    (make-worm (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 1)
                     (make-posn 1 0)
                     (make-posn 0 0))
               LEFT)
    (make-posn HCENTER VCENTER)))))

(define (final-scene g)
  (overlay/align
   "left" "bottom"
   (text (cond [(run-into-walls? (game-worm g)) "the worm hit the wall"]
               [(run-into-itself? (game-worm g)) "the worm ran into itself"])
         SEGMENT-SIZE "black")
   (render-game g)))

; Game -> Boolean
; stops the game if the worm has run into the walls or into itself
(define (end? g)
  (or (run-into-walls? (game-worm g))
      (run-into-itself? (game-worm g))))

; Food -> Food 
; creates a new piece of food that isn't located at p
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
   p (make-posn (random HSEGMENTS) (random VSEGMENTS))))
 
; Food Food -> Food 
; checks if p is located at the candidate position; if so, produces
; a piece of food located at some other position
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; Number -> Number
; starts the game using r as the rate at which the clock ticks
; returns the length of the final worm
(define (worm-main r)
  (length
   (worm-tail
    (game-worm
     (big-bang (make-game
                (make-worm
                 (list (make-posn HCENTER VCENTER))
                 RIGHT)
                (food-create (make-posn HCENTER VCENTER)))
       [stop-when end? final-scene]
       [to-draw render-game]
       [on-key control-game]
       [on-tick update-game r])))))