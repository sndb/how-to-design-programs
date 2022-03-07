;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex218) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Tail is one of:
; — '()
; — (cons Segment Tail)
; interpretation: a possibly empty sequence of "connected" segments

; Tail example
(define ex-tail (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)))

(define-struct worm [tail direction])
; A Worm is a structure:
;   (make-worm Tail Direction)
; interpretation: (make-worm t d) represents a worm with tail t moving in the direction d

; Worm example
(define ex-worm (make-worm ex-tail RIGHT))

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

; Worm -> Image
; renders w

(check-expect
 (render-worm (make-worm (list (make-posn 0 0)
                               (make-posn 0 1)
                               (make-posn 0 2))
                         RIGHT))
 (add-tail-to-image (list (make-posn 0 0)
                          (make-posn 0 1)
                          (make-posn 0 2))
                    SCENE))

(define (render-worm w)
  (add-tail-to-image (worm-tail w) SCENE))

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

(check-expect (run-into-walls?/segment (make-posn 0 0)) #false)
(check-expect (run-into-walls?/segment (make-posn HCENTER VCENTER)) #false)
(check-expect (run-into-walls?/segment (make-posn HCENTER VSEGMENTS)) #true)
(check-expect (run-into-walls?/segment (make-posn HSEGMENTS VCENTER)) #true)
(check-expect (run-into-walls?/segment (make-posn HSEGMENTS VSEGMENTS)) #true)

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

; Worm -> Image
; renders the final scene

(check-expect
 (final-scene (make-worm (list (make-posn 0 0)
                               (make-posn 1 0)
                               (make-posn 1 -1))
                         UP))
 (overlay/align
  "left" "bottom"
  (text "the worm hit the wall" SEGMENT-SIZE "black")
  (render-worm (make-worm (list (make-posn 0 0)
                                (make-posn 1 0)
                                (make-posn 1 -1))
                          UP))))

(check-expect
 (final-scene (make-worm (list (make-posn 0 0)
                               (make-posn 0 1)
                               (make-posn 1 1)
                               (make-posn 1 0)
                               (make-posn 0 0))
                         LEFT))
 (overlay/align
  "left" "bottom"
  (text "the worm ran into itself" SEGMENT-SIZE "black")
  (render-worm (make-worm (list (make-posn 0 0)
                                (make-posn 0 1)
                                (make-posn 1 1)
                                (make-posn 1 0)
                                (make-posn 0 0))
                          LEFT))))

(define (final-scene w)
  (overlay/align
   "left" "bottom"
   (text (cond [(run-into-walls? w) "the worm hit the wall"]
               [(run-into-itself? w) "the worm ran into itself"])
         SEGMENT-SIZE "black")
   (render-worm w)))

; Worm -> Boolean
; stops the game if the worm has run into the walls or into itself
(define (end? w)
  (or (run-into-walls? w) (run-into-itself? w)))

; Number -> Worm
; starts the game using r as the rate at which the clock ticks
(define (worm-main r)
  (big-bang (make-worm (list (make-posn 0 0)
                             (make-posn 0 1)
                             (make-posn 0 2))
                       RIGHT)
    [stop-when end? final-scene]
    [to-draw render-worm]
    [on-key control-worm]
    [on-tick move-worm r]))