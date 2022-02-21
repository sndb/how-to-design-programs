;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex193) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; a plain background image 
(define MT (empty-scene 50 50))

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))
 
(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; Image Polygon -> Image
; adds a corner of p to img

(check-expect
 (render-poly MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly img p)
  (render-line (connect-dots img p) (first p) (last p)))

(check-expect (render-poly-cons MT triangle-p)
              (render-poly MT triangle-p))
(check-expect (render-poly-cons MT square-p)
              (render-poly MT square-p))

(define (render-poly-cons img p)
  (connect-dots img (cons (last p) p)))

(check-expect (render-poly-add-at-end MT triangle-p)
              (render-poly MT triangle-p))
(check-expect (render-poly-add-at-end MT square-p)
              (render-poly MT square-p))

(define (render-poly-add-at-end img p)
  (connect-dots img (add-at-end p (first p))))
 
; Image NELoP -> Image
; connects the Posns in p in an image

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))
(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                20 10 20 20 "red")
               10 10 20 10 "red"))

(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into img

(check-expect (render-line MT (make-posn 10 10) (make-posn 20 20))
              (scene+line MT 10 10 20 20 "red"))

(define (render-line img p q)
  (scene+line img (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; Polygon -> Posn
; extracts the last item from p

(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))

(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Polygon Posn -> Polygon
; creates a new polygon by adding p to the end of l

(check-expect
 (add-at-end (list (make-posn 10 10)
                   (make-posn 20 10)
                   (make-posn 20 20))
             (make-posn 10 20))
 (list (make-posn 10 10)
       (make-posn 20 10)
       (make-posn 20 20)
       (make-posn 10 20)))
(check-expect
 (add-at-end (list (make-posn 10 10)
                   (make-posn 20 10)
                   (make-posn 20 20)
                   (make-posn 10 20))
             (make-posn 15 15))
 (list (make-posn 10 10)
       (make-posn 20 10)
       (make-posn 20 20)
       (make-posn 10 20)
       (make-posn 15 15)))

(define (add-at-end l p)
  (cond
    [(empty? (rest (rest (rest l))))
     (list (first l) (second l) (third l) p)]
    [else (cons (first l) (add-at-end (rest l) p))]))