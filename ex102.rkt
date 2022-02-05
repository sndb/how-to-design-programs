;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex102) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 200)
(define HEIGHT 200)
(define UFO-WIDTH 24)
(define UFO-HEIGHT 12)
(define TANK-WIDTH 20)
(define TANK-HEIGHT 8)
(define MISSILE-SIZE 8)
(define SURFACE-HEIGHT (/ HEIGHT 6))

(define TANK-SPEED 5)
(define UFO-DESCEND-SPEED 3)
(define UFO-JUMP-SPEED 3)
(define MISSILE-SPEED (* 3 UFO-DESCEND-SPEED))
(define HIT-PROXIMITY 10)

; graphical constants
(define UFO (overlay (circle (/ UFO-HEIGHT 2) "solid" "green")
                     (rectangle UFO-WIDTH (/ UFO-HEIGHT 3) "solid" "green")))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "darkgreen"))
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "darkblue"))
(define SURFACE (empty-scene WIDTH SURFACE-HEIGHT "lightbrown"))
(define BACKGROUND (above SCENE SURFACE))

; A UFO is a Posn.
; interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation: (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A MissileOrNot is one of:
; – #false
; – Posn
; interpretation: #false means the missile is in the tank;
; Posn says the missile is at that location

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation: represents the complete state of a
; space invader game

; data examples
(define ex1 (make-sigs (make-posn 20 10)
                       (make-tank 28 -3)
                       #false))
(define ex1-scene
  (place-image TANK
               28 HEIGHT
               (place-image UFO
                            20 10
                            BACKGROUND)))

(define ex2 (make-sigs (make-posn 20 10)
                       (make-tank 28 -3)
                       (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define ex2-scene
  (place-image TANK
               28 HEIGHT
               (place-image UFO
                            20 10
                            (place-image MISSILE
                                         28 (- HEIGHT TANK-HEIGHT)
                                         BACKGROUND))))

(define ex3 (make-sigs (make-posn 20 100)
                       (make-tank 100 3)
                       (make-posn 22 103)))
(define ex3-scene
  (place-image TANK
               100 HEIGHT
               (place-image UFO
                            20 100
                            (place-image MISSILE
                                         22 103
                                         BACKGROUND))))

; SIGS.v2 -> Image
; renders the given game state on top of BACKGROUND
(check-expect (si-render ex1) ex1-scene)
(check-expect (si-render ex2) ex2-scene)
(check-expect (si-render ex3) ex3-scene)
(define (si-render s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missile-render (sigs-missile s)
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

; MissileOrNot Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render #false SCENE) SCENE)
(check-expect
 (missile-render (make-posn 32 (- HEIGHT TANK-HEIGHT 10)) SCENE)
 (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10) SCENE))
(define (missile-render m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 -> Boolean
; stops the game if the ufo lands or if the missile hits the ufo
(check-expect
 (si-game-over? (make-sigs (make-posn 20 10)
                           (make-tank 28 -3)
                           #false))
 #false)
(check-expect
 (si-game-over? (make-sigs (make-posn 20 10)
                           (make-tank 28 -3)
                           (make-posn 28 (- HEIGHT TANK-HEIGHT))))
 #false)
(check-expect
 (si-game-over? (make-sigs (make-posn 20 100)
                           (make-tank 100 3)
                           (make-posn 22 103)))
 #true)
(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))
      (missile-hit? (sigs-ufo s) (sigs-missile s))))

; UFO -> Boolean
; checks if the ufo has landed
(check-expect (ufo-landed? (make-posn 20 100)) #false)
(check-expect (ufo-landed? (make-posn 20 HEIGHT)) #true)
(define (ufo-landed? u)
  (>= (posn-y u) HEIGHT))

; UFO MissileOrNot -> Boolean
; checks if the missile has hit the ufo
(check-expect
 (missile-hit? (make-posn 20 10) (make-posn 28 (- HEIGHT TANK-HEIGHT)))
 #false)
(check-expect
 (missile-hit? (make-posn 20 100) (make-posn 22 103))
 #true)
(define (missile-hit? u m)
  (cond
    [(boolean? m) #false]
    [(posn? m) (> HIT-PROXIMITY (posn-proximity u m))]))

; Posn Posn -> Number
; computes the proximity of p1 and p2
(check-expect (posn-proximity (make-posn 0 0) (make-posn 0 0)) 0)
(check-expect (posn-proximity (make-posn 5 0) (make-posn 0 0)) 5)
(check-expect (posn-proximity (make-posn 0 5) (make-posn 0 0)) 5)
(check-within (posn-proximity (make-posn 5 5) (make-posn 0 0))
              (posn-magnitude (make-posn 5 5))
              0.001)
(check-within (posn-proximity (make-posn 5 5) (make-posn 2 7))
              (posn-magnitude (posn-difference (make-posn 5 5)
                                               (make-posn 2 7)))
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
(check-expect (posn-difference (make-posn 5 5) (make-posn 2 7))
              (make-posn 3 -2))
(define (posn-difference p1 p2)
  (make-posn (- (posn-x p1) (posn-x p2)) (- (posn-y p1) (posn-y p2))))

; SIGS.v2 -> Image
; renders the final state of the game
(check-expect
 (si-render-final (make-sigs (make-posn 20 HEIGHT)
                             (make-tank 28 -3)
                             #false))
 (overlay (text "You lost." 24 "red")
          (si-render (make-sigs (make-posn 20 HEIGHT)
                                (make-tank 28 -3)
                                #false))))
(check-expect
 (si-render-final (make-sigs (make-posn 20 HEIGHT)
                             (make-tank 28 -3)
                             (make-posn 28 (- HEIGHT TANK-HEIGHT))))
 (overlay (text "You lost." 24 "red")
          (si-render (make-sigs (make-posn 20 HEIGHT)
                                (make-tank 28 -3)
                                (make-posn 28 (- HEIGHT TANK-HEIGHT))))))
(check-expect
 (si-render-final (make-sigs (make-posn 20 100)
                             (make-tank 100 3)
                             (make-posn 22 103)))
 (overlay (text "You won!" 24 "green")
          (si-render (make-sigs (make-posn 20 100)
                                (make-tank 100 3)
                                (make-posn 22 103)))))
(define (si-render-final s)
  (cond
    [(ufo-landed? (sigs-ufo s))
     (overlay (text "You lost." 24 "red") (si-render s))]
    [else (overlay (text "You won!" 24 "green") (si-render s))]))

; SIGS.v2 -> SIGS.v2
; determines to which position the objects move now
(define (si-move s)
  (si-move-proper s (- (random UFO-JUMP-SPEED) (/ 2 UFO-JUMP-SPEED))))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta
(check-expect (si-move-proper (make-sigs (make-posn 20 100)
                                         (make-tank 100 3)
                                         #false)
                              5)
              (make-sigs (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
                         (make-tank 103 3)
                         #false))
(check-expect (si-move-proper (make-sigs (make-posn 20 100)
                                         (make-tank 100 3)
                                         (make-posn 100 120)) 5)
              (make-sigs (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
                         (make-tank 103 3)
                         (make-posn 100 (- 120 MISSILE-SPEED))))
(check-expect (si-move-proper (make-sigs (make-posn 20 100)
                                         (make-tank 100 3)
                                         (make-posn 100 -20)) 5)
              (make-sigs (make-posn 25 (+ 100 UFO-DESCEND-SPEED))
                         (make-tank 103 3)
                         #false))
(define (si-move-proper s delta)
  (make-sigs (ufo-move (sigs-ufo s) delta)
             (tank-move (sigs-tank s))
             (missile-move (sigs-missile s))))

; MissileOrNot -> Boolean
; checks if the missile is visible
(check-expect (missile-visible? #false) #false)
(check-expect (missile-visible? (make-posn 50 0)) #false)
(check-expect (missile-visible? (make-posn 50 1)) #true)
(check-expect (missile-visible? (make-posn 50 HEIGHT)) #true)
(define (missile-visible? m)
  (cond
    [(boolean? m) #false]
    [(posn? m) (> (posn-y m) 0)]))

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

; MissileOrNot -> MissileOrNot
; moves the missile
(check-expect (missile-move #false) #false)
(check-expect (missile-move (make-posn 100 0)) #false)
(check-expect (missile-move (make-posn 100 100))
              (make-posn 100 (- 100 MISSILE-SPEED)))
(define (missile-move m)
  (cond
    [(boolean? m) m]
    [(posn? m)
     (if (missile-visible? m)
         (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))
         #false)]))

; SIGS.v2 KeyEvent -> SIGS.v2
; handles key events
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 5) #false) "up")
 (make-sigs (make-posn 100 100) (make-tank 100 5) #false))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 5) #false) "right")
 (make-sigs (make-posn 100 100) (make-tank 100 5) #false))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 5) #false) "left")
 (make-sigs (make-posn 100 100) (make-tank 100 -5) #false))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 -5) #false) "right")
 (make-sigs (make-posn 100 100) (make-tank 100 5) #false))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 -5) #false) "left")
 (make-sigs (make-posn 100 100) (make-tank 100 -5) #false))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 -5) #false) " ")
 (make-sigs (make-posn 100 100) (make-tank 100 -5) (make-posn 100 HEIGHT)))
(check-expect
 (si-control
  (make-sigs (make-posn 100 100) (make-tank 100 -5) (make-posn 100 100)) " ")
 (make-sigs (make-posn 100 100) (make-tank 100 -5) (make-posn 100 100)))
(define (si-control s ke)
  (cond
    [(string=? "left" ke)
     (make-sigs (sigs-ufo s) (tank-turn-left (sigs-tank s)) (sigs-missile s))]
    [(string=? "right" ke)
     (make-sigs (sigs-ufo s) (tank-turn-right (sigs-tank s)) (sigs-missile s))]
    [(string=? " " ke)
     (make-sigs (sigs-ufo s) (sigs-tank s) (tank-fire-missile (sigs-tank s)
                                                              (sigs-missile s)))]
    [else s]))

; Tank -> Tank
; turns left
(define (tank-turn-left t)
  (make-tank (tank-loc t)
             (if (< 0 (tank-vel t))
                 (- 0 (tank-vel t))
                 (tank-vel t))))

; Tank -> Tank
; turns right
(define (tank-turn-right t)
  (make-tank (tank-loc t)
             (if (> 0 (tank-vel t))
                 (- 0 (tank-vel t))
                 (tank-vel t))))

; Tank MissileOrNot -> MissileOrNot
; fires the missile from the tank
(define (tank-fire-missile t m)
  (cond
    [(boolean? m) (make-posn (tank-loc t) HEIGHT)]
    ([posn? m] m)))

; Number -> SIGS.v2
; launches the game with tank at x
(define (si-main x)
  (big-bang (make-sigs (make-posn (/ WIDTH 2) 0)
                       (make-tank x TANK-SPEED)
                       #false)
    [on-tick si-move 0.1]
    [to-draw si-render]
    [stop-when si-game-over? si-render-final]
    [on-key si-control]))