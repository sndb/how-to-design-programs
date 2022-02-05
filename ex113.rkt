;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex113) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A UFO is a Posn.
; interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation: (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation: (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation: represents the complete state of a
; space invader game

(define u (make-posn 42 64))
(define t (make-tank 57 3))
(define m (make-posn 43 56))

; Any -> Boolean
; is v an element of the SIGS collection
(check-expect (sigs? (make-aim u t)) #true)
(check-expect (sigs? (make-fired u t m)) #true)
(check-expect (sigs? #false) #false)
(check-expect (sigs? (make-posn 9 2)) #false)
(check-expect (sigs? "yellow") #false)
(check-expect (sigs? #true) #false)
(check-expect (sigs? 10) #false)
(check-expect (sigs? empty-image) #false)
(define (sigs? v)
  (or (aim? v) (fired? v)))

; A Coordinate is one of:
; – a NegativeNumber
; interpretation on the y axis, distance from top
; – a PositiveNumber
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean
; is v an element of the Coordinate collection
(check-expect (coordinate? 12) #true)
(check-expect (coordinate? -4) #true)
(check-expect (coordinate? (make-posn 9 2)) #true)
(check-expect (coordinate? 0) #false)
(check-expect (coordinate? (make-aim u t)) #false)
(check-expect (coordinate? (make-fired u t m)) #false)
(check-expect (coordinate? #false) #false)
(check-expect (coordinate? "yellow") #false)
(check-expect (coordinate? #true) #false)
(check-expect (coordinate? empty-image) #false)
(define (coordinate? v)
  (or (and (real? v)
           (or (positive? v) (negative? v)))
      (posn? v)))

(define-struct vcat [position happiness])
; A VCat is a structure:
;   (make-vcat Number Number)
; interpretation: cat's position (in pixels between the left border of the scene
; and the right edge of the cat) and happiness level

(define-struct vcham [color position happiness])
; A VCham is a structure:
;   (make-vcham Color Number Number)
; interpretation: chameleon's color, position (in pixels between the left border
; of the scene and the right edge of the chameleon), and happiness level

; A VAnimal is either
; – a VCat
; – a VCham

; Any -> Boolean
; is v an element of the VAnimal collection
(check-expect (vanimal? (make-vcat 42 86)) #true)
(check-expect (vanimal? (make-vcham "red" 42 86)) #true)
(check-expect (vanimal? 12) #false)
(check-expect (vanimal? -4) #false)
(check-expect (vanimal? (make-posn 9 2)) #false)
(check-expect (vanimal? 0) #false)
(check-expect (vanimal? (make-aim u t)) #false)
(check-expect (vanimal? (make-fired u t m)) #false)
(check-expect (vanimal? #false) #false)
(check-expect (vanimal? "yellow") #false)
(check-expect (vanimal? #true) #false)
(check-expect (vanimal? empty-image) #false)
(define (vanimal? v)
  (or (vcat? v) (vcham? v)))