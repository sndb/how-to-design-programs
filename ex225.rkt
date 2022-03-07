;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex225) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; physical constants
(define WIDTH 240)
(define HEIGHT 360)

(define GROUND-HEIGHT 30)
(define FIRE-RADIUS 8)
(define TREE-RADIUS 10)
(define WATER-WIDTH 20)
(define AIRPLANE-SIDE-LENGTH 20)
(define TREE-SPACING (* TREE-RADIUS 2.2))

(define TICKRATE 0.1)
(define EXTINGUISH-DISTANCE WATER-WIDTH)
(define WATER-SPEED (/ WATER-WIDTH 2))

(define AIRPLANE-Y (* 1/5 HEIGHT))
(define FIRE-Y (- HEIGHT GROUND-HEIGHT TREE-RADIUS))
(define TREE-Y (- HEIGHT GROUND-HEIGHT))


; graphical constants
(define GROUND
  (rectangle (- WIDTH 1) (- GROUND-HEIGHT 1) "solid" "darkgreen"))
(define FIRE
  (overlay/offset (overlay (circle (* 5/7 FIRE-RADIUS) "solid" "yellow")
                           (circle FIRE-RADIUS "solid" "darkorange"))
                  0
                  (- FIRE-RADIUS)
                  (triangle (* 2 FIRE-RADIUS) "solid" "darkorange")))
(define WATER
  (ellipse WATER-WIDTH (/ WATER-WIDTH 2) "solid" "blue"))
(define TREE
  (overlay/offset
   (circle TREE-RADIUS "solid" "forestgreen")
   0
   (/ TREE-RADIUS 2)
   (rectangle (/ TREE-RADIUS 4) (* TREE-RADIUS 2) "solid" "brown")))
(define AIRPLANE (rhombus AIRPLANE-SIDE-LENGTH 160 "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "skyblue"))
(define BACKGROUND
  (place-images (make-list 15 TREE)
                (list (make-posn (* TREE-SPACING 0)  TREE-Y)
                      (make-posn (* TREE-SPACING 1)  TREE-Y)
                      (make-posn (* TREE-SPACING 2)  TREE-Y)
                      (make-posn (* TREE-SPACING 3)  TREE-Y)
                      (make-posn (* TREE-SPACING 4)  TREE-Y)
                      (make-posn (* TREE-SPACING 5)  TREE-Y)
                      (make-posn (* TREE-SPACING 6)  TREE-Y)
                      (make-posn (* TREE-SPACING 7)  TREE-Y)
                      (make-posn (* TREE-SPACING 8)  TREE-Y)
                      (make-posn (* TREE-SPACING 9)  TREE-Y)
                      (make-posn (* TREE-SPACING 10) TREE-Y)
                      (make-posn (* TREE-SPACING 11) TREE-Y)
                      (make-posn (* TREE-SPACING 12) TREE-Y)
                      (make-posn (* TREE-SPACING 13) TREE-Y)
                      (make-posn (* TREE-SPACING 14) TREE-Y))
                (place-image GROUND
                             (/ WIDTH 2)
                             (- HEIGHT (/ GROUND-HEIGHT 2))
                             SCENE)))


; world state
(define-struct game [airplane fires waters])
(define-struct airplane [position velocity])

; An Airplane is a structure:
;   (make-airplane Number Number)
; interpretation: (make-airplane x dx) represents the distance x from
; the left border of the scene to the center of the plane, and the
; velocity dx

; A Fire is an N.
; intepretation: represents the distance from the left border of the
; scene to the center of the fire

; A Fires is one of:
; '()
; (cons Fire Fires)

; A Water is a Posn.
; interpretation: (make-posn x y) represents the distance x from the
; left border of the scene and the distance y from the top of the scene
; to the center of the water load

; A Waters is one of:
; '()
; (cons Water Waters)

; A Game is a structure:
;   (make-game Airplane Fires Waters)
; interpretation: represents the game state

; examples
(define airplane-center (make-airplane (/ WIDTH 2) 5))
(define airplane-center-dx- (make-airplane (/ WIDTH 2) -5))
(define airplane-center-n1 (make-airplane (+ 5 (/ WIDTH 2)) 5))
(define airplane-negative (make-airplane -10 5))
(define airplane-at-left (make-airplane 0 5))
(define airplane-overwidth (make-airplane (+ 10 WIDTH) 5))
(define airplane-at-right (make-airplane WIDTH 5))
(define fire-one 42)
(define fire-two 85)
(define fires-two (list fire-one fire-two))
(define fires-one (list 85))
(define water-center (make-posn (airplane-position airplane-center)
                                AIRPLANE-Y))
(define water-center-n1 (make-posn (airplane-position airplane-center)
                                   (+ AIRPLANE-Y WATER-SPEED)))
(define water-on-fire (make-posn 42 FIRE-Y))
(define water-near-fire
  (make-posn (+ 42 3) (+ FIRE-Y 3)))
(define water-near-fire-n1
  (make-posn (+ 42 3) (+ FIRE-Y 3 WATER-SPEED)))
(define water-invisible (make-posn (/ WIDTH 2) (+ 10 HEIGHT)))
(define game-empty (make-game airplane-center '() '()))
(define game-empty-dx- (make-game airplane-center-dx- '() '()))
(define game-empty-n1 (make-game airplane-center-n1 '() '()))
(define game-airplane-negative (make-game airplane-negative '() '()))
(define game-airplane-overwidth (make-game airplane-overwidth '() '()))
(define game-airplane-at-left (make-game airplane-at-left '() '()))
(define game-airplane-at-right (make-game airplane-at-right '() '()))
(define game-two-fires (make-game airplane-center fires-two '()))
(define game-water-with-two-fires
  (make-game airplane-center fires-two (list water-near-fire)))
(define game-water-with-two-fires-n1
  (make-game airplane-center-n1 fires-one (list water-near-fire-n1)))
(define game-water-with-fire
  (make-game airplane-center fires-one (list water-near-fire)))


; Game -> Image
; renders the game

(check-expect
 (game-render game-empty)
 (add-airplane (game-airplane game-empty) BACKGROUND))
(check-expect
 (game-render game-two-fires)
 (add-airplane (game-airplane game-two-fires)
               (add-fires (game-fires game-two-fires)
                          BACKGROUND)))
(check-expect
 (game-render game-water-with-fire)
 (add-airplane (game-airplane game-water-with-fire)
               (add-fires (game-fires game-water-with-fire)
                          (add-waters (game-waters game-water-with-fire)
                                      BACKGROUND))))

(define (game-render g)
  (add-airplane (game-airplane g)
                (add-fires (game-fires g)
                           (add-waters (game-waters g)
                                       BACKGROUND))))


; Waters Image -> Image

(check-expect (add-waters '() BACKGROUND) BACKGROUND)
(check-expect (add-waters (list water-center water-on-fire) BACKGROUND)
              (add-water water-center
                         (add-water water-on-fire
                                    BACKGROUND)))

(define (add-waters w img)
  (cond
    [(empty? w) img]
    [else
     (add-water (first w) (add-waters (rest w) img))]))


; Water Image -> Image

(check-expect (add-water water-center BACKGROUND)
              (place-image WATER
                           (posn-x water-center)
                           (posn-y water-center)
                           BACKGROUND))

(define (add-water w img)
  (place-image WATER (posn-x w) (posn-y w) img))


; Fires Image -> Image

(check-expect (add-fires '() BACKGROUND) BACKGROUND)
(check-expect (add-fires fires-two BACKGROUND)
              (add-fire (first fires-two)
                        (add-fire (second fires-two)
                                  BACKGROUND)))

(define (add-fires f img)
  (cond
    [(empty? f) img]
    [else
     (add-fire (first f) (add-fires (rest f) img))]))


; Fire Image -> Image

(check-expect (add-fire 42 BACKGROUND)
              (place-image FIRE 42 FIRE-Y BACKGROUND))

(define (add-fire f img)
  (place-image FIRE f FIRE-Y img))


; Airplane Image -> Image

(check-expect
 (add-airplane airplane-center BACKGROUND)
 (place-image AIRPLANE
              (airplane-position airplane-center)
              AIRPLANE-Y
              BACKGROUND))

(define (add-airplane a img)
  (place-image AIRPLANE
               (airplane-position a)
               AIRPLANE-Y
               img))


; Game KeyEvent -> Game
; controls the game:
; left and right - turns the airplane
; space - drops the water load

(check-expect (game-control game-empty "a") game-empty)
(check-expect (game-control game-empty "left") game-empty-dx-)
(check-expect (game-control game-empty-dx- "right") game-empty)

(define (game-control g ke)
  (cond
    [(key=? "left" ke) (make-game (turn-left (game-airplane g))
                                  (game-fires g)
                                  (game-waters g))]
    [(key=? "right" ke) (make-game (turn-right (game-airplane g))
                                   (game-fires g)
                                   (game-waters g))]
    [(key=? " " ke) (make-game (game-airplane g)
                               (game-fires g)
                               (drop-water (game-airplane g)
                                           (game-waters g)))]
    [else g]))

; Airplane Waters -> Waters
; adds a new water to w

(check-expect (drop-water airplane-center '())
              (list water-center))
(check-expect (drop-water airplane-center (list water-on-fire))
              (list water-center water-on-fire))

(define (drop-water a w)
  (cons (make-posn (airplane-position a) AIRPLANE-Y) w))


; Airplane -> Airplane
; set the velocity of the airplane to a negative value

(check-expect (turn-left airplane-center) airplane-center-dx-)
(check-expect (turn-left airplane-center-dx-) airplane-center-dx-)

(define (turn-left a)
  (make-airplane (airplane-position a)
                 (if (positive? (airplane-velocity a))
                     (- (airplane-velocity a))
                     (airplane-velocity a))))


; Airplane -> Airplane
; set the velocity of the airplane to a positive value

(check-expect (turn-right airplane-center) airplane-center)
(check-expect (turn-right airplane-center-dx-) airplane-center)

(define (turn-right a)
  (make-airplane (airplane-position a)
                 (if (negative? (airplane-velocity a))
                     (- (airplane-velocity a))
                     (airplane-velocity a))))


; Game -> Game
; updates the game:
; adds the airplane's velocity to its position

(check-expect (game-update game-empty) game-empty-n1)
(check-expect (game-update game-airplane-negative)
              game-airplane-at-left)
(check-expect (game-update game-airplane-overwidth)
              game-airplane-at-right)
(check-expect (game-update game-water-with-two-fires)
              game-water-with-two-fires-n1)

(define (game-update g)
  (make-game (airplane-update (game-airplane g))
             (fires-update (game-fires g) (game-waters g))
             (waters-update (game-waters g))))


; Fires Waters -> Fires
; extinguish the fires that are near the waters

(check-expect (fires-update fires-two
                            (list water-near-fire water-center))
              fires-one)

(define (fires-update f w)
  (cond
    [(empty? f) '()]
    [else
     (if (fire-extinguished? (first f) w)
         (fires-update (rest f) w)
         (cons (first f) (fires-update (rest f) w)))]))


; Fire Waters -> Boolean
; is f extinguished by w?

(check-expect (fire-extinguished? fire-one (list water-near-fire
                                                 water-center))
              #true)
(check-expect (fire-extinguished? fire-two (list water-near-fire
                                                 water-center))
              #false)

(define (fire-extinguished? f w)
  (cond
    [(empty? w) #false]
    [else
     (or (can-extinguish? (first w) f)
         (fire-extinguished? f (rest w)))]))


; Water Fire -> Boolean
; can w extinguish f?

(check-expect (can-extinguish? water-near-fire fire-one) #true)
(check-expect (can-extinguish? water-near-fire fire-two) #false)

(define (can-extinguish? w f)
  (<= (distance w f) EXTINGUISH-DISTANCE))


; Water Fire -> Number
; computes distance between w and f

(check-expect (distance water-on-fire fire-one) 0)
(check-within (distance (make-posn 50 (- FIRE-Y 5)) 45)
              7.071
              0.01)

(define (distance w f)
  (sqrt (+ (sqr (- (posn-x w) f)) (sqr (- (posn-y w) FIRE-Y)))))


; Waters -> Waters
; moves the waters down by WATER-SPEED, removing if not visible

(check-expect (waters-update '()) '())
(check-expect (waters-update (list water-center))
              (list water-center-n1))
(check-expect (waters-update (list water-center
                                   water-invisible))
              (list water-center-n1))

(define (waters-update w)
  (cond
    [(empty? w) '()]
    [else
     (if (water-visible? (first w))
         (cons (water-update (first w)) (waters-update (rest w)))
         (waters-update (rest w)))]))


; Water -> Water
; moves the water down by WATER-SPEED

(check-expect (water-update water-center) water-center-n1)

(define (water-update w)
  (make-posn (posn-x w) (+ WATER-SPEED (posn-y w))))


; Water -> Boolean
; is w visible?

(check-expect (water-visible? water-invisible) #false)
(check-expect (water-visible? water-center) #true)

(define (water-visible? w)
  (> HEIGHT (posn-y w)))


; Aircraft -> Aircraft
; adds the airplane's velocity to its position, clamping at the
; world borders

(check-expect (airplane-update airplane-center) airplane-center-n1)

(define (airplane-update a)
  (make-airplane
   (clamp-position (+ (airplane-position a) (airplane-velocity a)))
   (airplane-velocity a)))


; Number -> Number
; clamps n between the world borders

(check-expect (clamp-position -5) 0)
(check-expect (clamp-position 0) 0)
(check-expect (clamp-position (+ WIDTH 10)) WIDTH)

(define (clamp-position n)
  (cond
    [(> 0 n) 0]
    [(> n WIDTH) WIDTH]
    [else n]))


; Game -> Boolean
; determines if the number of current fires is 0

(check-expect (game-stop? game-empty) #true)
(check-expect (game-stop? game-two-fires) #false)
(check-expect (game-stop? game-water-with-fire) #false)

(define (game-stop? g)
  (zero? (length (game-fires g))))


; Game -> Image
; renders the final scene

(check-expect (final-scene game-empty)
              (overlay (text "You won!" 24 "green")
                       (game-render game-empty)))
(check-expect (final-scene game-two-fires)
              (overlay (text "You lost." 24 "red")
                       (game-render game-two-fires)))
(check-expect (final-scene game-water-with-fire)
              (overlay (text "You lost." 24 "red")
                       (game-render game-water-with-fire)))

(define (final-scene g)
  (if (game-stop? g)
      (overlay (text "You won!" 24 "green") (game-render g))
      (overlay (text "You lost." 24 "red") (game-render g))))


; N -> Fires
; starts n fires

(check-random (start-fires 0) '())
(check-random (start-fires 1) (start-fire '()))
(check-random (start-fires 4) (start-fire
                               (start-fire
                                (start-fire
                                 (start-fire
                                  '())))))

(define (start-fires n)
  (cond
    [(zero? n) '()]
    [else
     (start-fire (start-fires (sub1 n)))]))


; Fires -> Fires
; starts a new fire

(check-random (start-fire '()) (list (random WIDTH)))
(check-random (start-fire fires-two) (list (random WIDTH)
                                           (first fires-two)
                                           (second fires-two)))

(define (start-fire f)
  (cons (random WIDTH) f))


; N Number -> Game
; starts the game with n initial fires and t seconds to
; extinguish all of them
(define (game-start n t)
  (big-bang (make-game airplane-center (start-fires n) '())
    [to-draw game-render]
    [on-key game-control]
    [on-tick game-update TICKRATE (/ t TICKRATE)]
    [stop-when game-stop? final-scene]))