;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex224) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; physical constants
(define WIDTH 240)
(define HEIGHT 360)

(define TANK-SPEED 5)
(define UFO-DESCEND-SPEED 3)
(define UFO-JUMP-SPEED 3)
(define MISSILE-SPEED (* 3 UFO-DESCEND-SPEED))
(define HIT-PROXIMITY 10)

(define UFO-WIDTH 24)
(define UFO-HEIGHT 12)
(define TANK-WIDTH 20)
(define TANK-HEIGHT 8)
(define MISSILE-SIZE 8)
(define SURFACE-HEIGHT (/ HEIGHT 6))


; graphical constants
(define UFO
  (overlay (circle (/ UFO-HEIGHT 2) "solid" "green")
           (rectangle UFO-WIDTH (/ UFO-HEIGHT 3) "solid" "green")))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "darkgreen"))
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "darkblue"))
(define SURFACE (empty-scene WIDTH SURFACE-HEIGHT "lightbrown"))
(define BACKGROUND (above SCENE SURFACE))


(define-struct tank [loc vel])
(define-struct game [ufo tank missiles])

; A UFO is a Posn.
; interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

; A Tank is a structure:
;   (make-tank Number Number).
; interpretation: (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation: (make-posn x y) is the missile's place

; A LMissiles is one of:
; — '()
; — (cons Missle LMissiles)

; A Game is a structure:
;   (make-game UFO Tank LMissiles)
; interpretation: represents the complete state of a
; space invader game

; examples
(define ex-u (make-posn 42 64))
(define ex-t (make-tank 57 3))
(define ex-m (make-posn 43 56))
(define ex-g (make-game ex-u ex-t (list ex-m)))


; Game -> Image
; renders the given game state on top of BACKGROUND
(define (si-render g)
  (tank-render (game-tank g)
               (ufo-render (game-ufo g)
                           (missiles-render (game-missiles g)
                                            BACKGROUND))))


; Tank Image -> Image
; adds t to the given image im

(check-expect (tank-render (make-tank 28 -3) BACKGROUND)
              (place-image TANK 28 HEIGHT BACKGROUND))

(define (tank-render t im)
  (place-image TANK (tank-loc t) HEIGHT im))


; UFO Image -> Image
; adds u to the given image im

(check-expect (ufo-render (make-posn 20 10) BACKGROUND)
              (place-image UFO 20 10 BACKGROUND))

(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))


; Missile Image -> Image
; adds m to the given image im

(check-expect (missile-render (make-posn 22 103) BACKGROUND)
              (place-image MISSILE 22 103 BACKGROUND))

(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))


; LMissiles Image -> Image
; adds l to the given image im

(check-expect (missiles-render '() BACKGROUND) BACKGROUND)

(check-expect
 (missiles-render (list (make-posn 22 103)) BACKGROUND)
 (place-image MISSILE 22 103 BACKGROUND))

(check-expect
 (missiles-render (list (make-posn 22 103)
                        (make-posn 42 64))
                  BACKGROUND)
 (place-image MISSILE 22 103
              (place-image MISSILE 42 64
                           BACKGROUND)))

(define (missiles-render l im)
  (cond
    [(empty? l) BACKGROUND]
    [else
     (missile-render (first l) (missiles-render (rest l) im))]))


; Game -> Boolean
; stops the game if the ufo lands or if the missile hits the ufo

(check-expect
 (si-game-over?
  (make-game (make-posn 20 10) (make-tank 28 -3) '()))
 #false)

(check-expect
 (si-game-over?
  (make-game (make-posn 20 10)
             (make-tank 28 -3)
             (list (make-posn 28 (- HEIGHT TANK-HEIGHT)))))
 #false)

(check-expect
 (si-game-over?
  (make-game (make-posn 20 100)
             (make-tank 100 3)
             (list (make-posn 22 103))))
 #true)

(define (si-game-over? g)
  (or (ufo-landed? (game-ufo g))
      (missiles-hit? (game-ufo g)
                     (game-missiles g))))

; UFO LMissiles -> Boolean

(check-expect
 (missiles-hit? (make-posn 20 10) '())
 #false)
(check-expect
 (missiles-hit? (make-posn 20 10)
                (list (make-posn 30 40)))
 #false)
(check-expect
 (missiles-hit? (make-posn 20 10)
                (list (make-posn 22 13)))
 #true)

(define (missiles-hit? u l)
  (cond
    [(empty? l) #false]
    [else
     (or (missile-hit? u (first l))
         (missiles-hit? u (rest l)))]))


; UFO -> Boolean
; checks if the ufo has landed

(check-expect (ufo-landed? (make-posn 20 100)) #false)
(check-expect (ufo-landed? (make-posn 20 HEIGHT)) #true)

(define (ufo-landed? u)
  (>= (posn-y u) HEIGHT))


; UFO Missile -> Boolean
; checks if the missile has hit the ufo

(check-expect
 (missile-hit?
  (make-posn 20 10) (make-posn 28 (- HEIGHT TANK-HEIGHT)))
 #false)
(check-expect
 (missile-hit? (make-posn 20 100) (make-posn 22 103))
 #true)

(define (missile-hit? u m)
  (> HIT-PROXIMITY (posn-proximity u m)))


; Posn Posn -> Number
; computes the proximity of p1 and p2

(check-expect (posn-proximity (make-posn 0 0) (make-posn 0 0)) 0)
(check-expect (posn-proximity (make-posn 5 0) (make-posn 0 0)) 5)
(check-expect (posn-proximity (make-posn 0 5) (make-posn 0 0)) 5)
(check-within (posn-proximity (make-posn 5 5) (make-posn 0 0))
              (posn-magnitude (make-posn 5 5))
              0.001)
(check-within (posn-proximity (make-posn 5 5) (make-posn 2 7))
              (posn-magnitude (posn-difference
                               (make-posn 5 5) (make-posn 2 7)))
              0.001)

(define (posn-proximity p1 p2)
  (posn-magnitude (posn-difference p1 p2)))


; Posn -> Number
; computes the magnitude of p

(check-expect (posn-magnitude (make-posn 0 0)) 0)
(check-expect (posn-magnitude (make-posn 5 0)) 5)
(check-expect (posn-magnitude (make-posn 3 4)) 5)

(define (posn-magnitude p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))


; Posn Posn -> Posn
; computes the difference between p1 and p2

(check-expect
 (posn-difference (make-posn 5 5) (make-posn 2 7))
 (make-posn 3 -2))

(define (posn-difference p1 p2)
  (make-posn (- (posn-x p1) (posn-x p2))
             (- (posn-y p1) (posn-y p2))))


; Game -> Image
; renders the final state of the game

(check-expect
 (si-render-final
  (make-game (make-posn 20 HEIGHT)
             (make-tank 28 -3)
             '()))
 (overlay (text "You lost." 24 "red")
          (si-render (make-game (make-posn 20 HEIGHT)
                                (make-tank 28 -3)
                                '()))))

(check-expect
 (si-render-final
  (make-game (make-posn 20 HEIGHT)
             (make-tank 28 -3)
             (list (make-posn 28 (- HEIGHT TANK-HEIGHT)))))
 (overlay (text "You lost." 24 "red")
          (si-render (make-game
                      (make-posn 20 HEIGHT)
                      (make-tank 28 -3)
                      (list (make-posn 28 (- HEIGHT TANK-HEIGHT)))))))

(check-expect
 (si-render-final
  (make-game (make-posn 20 100)
             (make-tank 100 3)
             (list (make-posn 22 103))))
 (overlay (text "You won!" 24 "green")
          (si-render (make-game (make-posn 20 100)
                                (make-tank 100 3)
                                (list (make-posn 22 103))))))

(define (si-render-final g)
  (if (ufo-landed? (game-ufo g))
      (overlay (text "You lost." 24 "red") (si-render g))
      (overlay (text "You won!" 24 "green") (si-render g))))


; Game -> Game
; determines to which position the objects move now
(define (si-move g)
  (si-move-proper
   g (- (random UFO-JUMP-SPEED) (/ 2 UFO-JUMP-SPEED))))


; Game Number -> Game 
; moves the space-invader objects predictably by delta

(check-expect
 (si-move-proper
  (make-game (make-posn 20 100)
             (make-tank 100 3)
             '())
  5)
 (make-game (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
            (make-tank 103 3)
            '()))

(check-expect
 (si-move-proper
  (make-game (make-posn 20 100)
             (make-tank 100 3)
             (list (make-posn 100 120)))
  5)
 (make-game (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
            (make-tank 103 3)
            (list (make-posn 100 (- 120 MISSILE-SPEED)))))

(check-expect
 (si-move-proper
  (make-game (make-posn 20 100)
             (make-tank 100 3)
             (list (make-posn 100 -20)))
  5)
 (make-game (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
            (make-tank 103 3)
            '()))

(define (si-move-proper g delta)
  (make-game (ufo-move (game-ufo g) delta)
             (tank-move (game-tank g))
             (missiles-move (game-missiles g))))


; LMissiles -> LMissiles
; moves the missiles, removing them from l if not visible

(check-expect (missiles-move '()) '())
(check-expect
 (missiles-move (list (make-posn 100 120)))
 (list (make-posn 100 (- 120 MISSILE-SPEED))))
(check-expect
 (missiles-move
  (list (make-posn 100 -20)))
 '())

(define (missiles-move l)
  (cond
    [(empty? l) '()]
    [else
     (if (missile-visible? (first l))
         (cons (missile-move (first l)) (missiles-move (rest l)))
         (missiles-move (rest l)))]))


; Missile -> Boolean
; checks if the missile is visible

(check-expect (missile-visible? (make-posn 50 0)) #false)
(check-expect (missile-visible? (make-posn 50 1)) #true)
(check-expect (missile-visible? (make-posn 50 HEIGHT)) #true)

(define (missile-visible? m)
  (> (posn-y m) 0))


; UFO -> UFO
; moves the ufo by delta

(check-expect (ufo-move (make-posn 100 100) 5)
              (make-posn 105 (+ UFO-DESCEND-SPEED 100)))

(define (ufo-move u d)
  (make-posn (+ d (posn-x u)) (+ UFO-DESCEND-SPEED (posn-y u))))


; Tank -> Tank
; moves the tank

(check-expect (tank-move (make-tank 100 5)) (make-tank 105 5))
(check-expect (tank-move (make-tank 64 -4)) (make-tank 60 -4))

(define (tank-move t)
  (make-tank (+ (tank-vel t) (tank-loc t)) (tank-vel t)))


; Missile -> Missile
; moves the missile

(check-expect (missile-move (make-posn 100 100))
              (make-posn 100 (- 100 MISSILE-SPEED)))

(define (missile-move m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))


; Game KeyEvent -> Game
; handles key events

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 5) '())
  "up")
 (make-game (make-posn 100 100) (make-tank 100 5) '()))

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 5) '())
  "right")
 (make-game (make-posn 100 100) (make-tank 100 5) '()))

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 5) '())
  "left")
 (make-game (make-posn 100 100) (make-tank 100 -5) '()))

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 -5) '())
  "right")
 (make-game (make-posn 100 100) (make-tank 100 5) '()))

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 -5) '())
  "left")
 (make-game (make-posn 100 100) (make-tank 100 -5) '()))

(check-expect
 (si-control
  (make-game (make-posn 100 100) (make-tank 100 -5) '())
  " ")
 (make-game (make-posn 100 100)
            (make-tank 100 -5)
            (list (make-posn 100 HEIGHT))))

(check-expect
 (si-control
  (make-game (make-posn 100 100)
             (make-tank 100 -5)
             (list (make-posn 100 100)))
  " ")
 (make-game
  (make-posn 100 100)
  (make-tank 100 -5)
  (list (make-posn 100 HEIGHT)
        (make-posn 100 100))))

(define (si-control g ke)
  (cond
    [(key=? "left" ke)
     (make-game
      (game-ufo g) (tank-turn-left (game-tank g))
      (game-missiles g))]
    [(key=? "right" ke)
     (make-game
      (game-ufo g) (tank-turn-right (game-tank g))
      (game-missiles g))]
    [(key=? " " ke)
     (make-game
      (game-ufo g) (game-tank g)
      (cons (tank-fire-missile (game-tank g)) (game-missiles g)))]
    [else g]))


; Tank -> Tank
; turns left
(define (tank-turn-left t)
  (make-tank (tank-loc t) (if (< 0 (tank-vel t))
                              (- 0 (tank-vel t))
                              (tank-vel t))))


; Tank -> Tank
; turns right
(define (tank-turn-right t)
  (make-tank (tank-loc t) (if (> 0 (tank-vel t))
                              (- 0 (tank-vel t))
                              (tank-vel t))))


; Tank -> Missile
; fires the missile
(define (tank-fire-missile t)
  (make-posn (tank-loc t) HEIGHT))


; Number -> Game
; launches the game with tank at x
(define (si-main x)
  (big-bang
      (make-game (make-posn (/ WIDTH 2) 0)
                 (make-tank x TANK-SPEED)
                 '())
    [on-tick si-move 0.1]
    [to-draw si-render]
    [stop-when si-game-over? si-render-final]
    [on-key si-control]))