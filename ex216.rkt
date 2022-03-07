;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex216) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SEGMENT-SIZE 20)
(define SEGMENT-COLOR "red")
(define VSEGMENTS 24)
(define HSEGMENTS 48)
(define VCENTER (/ VSEGMENTS 2))
(define HCENTER (/ HSEGMENTS 2))

(define SEGMENT (circle (/ SEGMENT-SIZE 2) "solid" SEGMENT-COLOR))
(define SCENE (empty-scene (* HSEGMENTS SEGMENT-SIZE)
                           (* VSEGMENTS SEGMENT-SIZE)))


; A Segment is a Posn.
; interpretation: (make-posn x y) represents a segment at the position (x,y)

; Segment examples
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

(define-struct worm [segment direction])
; A Worm is a structure:
;   (make-worm Segment Direction)
; interpretation: (make-worm s d) represents a worm consisting of the segment s
; and moving to the direction d

; Worm examples
(define ex-worm0 (make-worm ex-segment0 RIGHT))
(define ex-worm1 (make-worm ex-segment1 UP))

; Worm -> Worm
; moves w 1 segment forward at its direction

(check-expect (move-worm (make-worm (make-posn 0 0) RIGHT))
              (make-worm (make-posn 1 0) RIGHT))
(check-expect (move-worm (make-worm (make-posn 1 0) LEFT))
              (make-worm (make-posn 0 0) LEFT))
(check-expect (move-worm (make-worm (make-posn 0 0) DOWN))
              (make-worm (make-posn 0 1) DOWN))
(check-expect (move-worm (make-worm (make-posn 0 1) UP))
              (make-worm (make-posn 0 0) UP))

(define (move-worm w)
  (make-worm (move-segment (worm-segment w) (worm-direction w))
             (worm-direction w)))

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

; Worm KeyEvent -> Worm
; changes the direction of w when arrow key is pressed

(check-expect (control-worm (make-worm (make-posn 0 0) RIGHT)
                            "a")
              (make-worm (make-posn 0 0) RIGHT))
(check-expect (control-worm (make-worm (make-posn 0 0) RIGHT)
                            "right")
              (make-worm (make-posn 0 0) RIGHT))
(check-expect (control-worm (make-worm (make-posn 0 0) RIGHT)
                            "left")
              (make-worm (make-posn 0 0) LEFT))
(check-expect (control-worm (make-worm (make-posn 0 0) RIGHT)
                            "up")
              (make-worm (make-posn 0 0) UP))
(check-expect (control-worm (make-worm (make-posn 0 0) RIGHT)
                            "down")
              (make-worm (make-posn 0 0) DOWN))

(define (control-worm w ke)
  (cond
    [(key=? "up" ke) (make-worm (worm-segment w) UP)]
    [(key=? "down" ke) (make-worm (worm-segment w) DOWN)]
    [(key=? "right" ke) (make-worm (worm-segment w) RIGHT)]
    [(key=? "left" ke) (make-worm (worm-segment w) LEFT)]
    [else w]))

; Worm -> Image
; renders w

(check-expect (render-worm (make-worm (make-posn HCENTER VCENTER) RIGHT))
              (place-image SEGMENT (sg->px HCENTER) (sg->px VCENTER) SCENE))

(define (render-worm w)
  (place-image SEGMENT
               (sg->px (posn-x (worm-segment w)))
               (sg->px (posn-y (worm-segment w)))
               SCENE))

; Number -> Number
; translates segment position to pixel offset

(check-expect (sg->px 0) (/ SEGMENT-SIZE 2))
(check-expect (sg->px 5) (+ (* 5 SEGMENT-SIZE) (/ SEGMENT-SIZE 2)))

(define (sg->px x)
  (+ (* x SEGMENT-SIZE) (/ SEGMENT-SIZE 2)))

; Worm -> Boolean
; is w has reached the walls of the world?

(check-expect (walls-reached? (make-worm (make-posn 0 0) UP))
              #false)
(check-expect (walls-reached? (make-worm (make-posn HCENTER VCENTER) UP))
              #false)
(check-expect (walls-reached? (make-worm (make-posn HCENTER VSEGMENTS) UP))
              #true)
(check-expect (walls-reached? (make-worm (make-posn HSEGMENTS VCENTER) UP))
              #true)
(check-expect (walls-reached? (make-worm (make-posn HSEGMENTS VSEGMENTS) UP))
              #true)

(define (walls-reached? w)
  (walls-reached?/segment (worm-segment w)))

; Segment -> Boolean
; is s has reached the walls of the world?

(check-expect (walls-reached?/segment (make-posn 0 0)) #false)
(check-expect (walls-reached?/segment (make-posn HCENTER VCENTER)) #false)
(check-expect (walls-reached?/segment (make-posn HCENTER VSEGMENTS)) #true)
(check-expect (walls-reached?/segment (make-posn HSEGMENTS VCENTER)) #true)
(check-expect (walls-reached?/segment (make-posn HSEGMENTS VSEGMENTS)) #true)

(define (walls-reached?/segment s)
  (or (not (and (<= 0 (posn-x s)) (< (posn-x s) HSEGMENTS)))
      (not (and (<= 0 (posn-y s)) (< (posn-y s) VSEGMENTS)))))

; Worm -> Image
; renders the final scene

(check-expect
 (final-scene (make-worm (make-posn 0 -1) UP))
 (overlay/align "left" "bottom"
                (text "worm hit border" 14 "black")
                (render-worm (make-worm (make-posn 0 -1) UP))))

(define (final-scene w)
  (overlay/align "left" "bottom"
                 (text "worm hit border" 14 "black")
                 (render-worm w)))

; Number -> Worm
; starts the game using r as the rate at which the clock ticks
(define (worm-main r)
  (big-bang (make-worm (make-posn HCENTER VCENTER) RIGHT)
    [stop-when walls-reached? final-scene]
    [to-draw render-worm]
    [on-key control-worm]
    [on-tick move-worm r]))