;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex258) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (local (; Image [NEList-of Posn] -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))
 
          ; Image Posn Posn -> Image 
          ; draws a red line from Posn p to Posn q into img
          (define (render-line img p q)
            (scene+line
             img (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))
          
          ; connect all dots between first and last
          (define almost-connected
            (connect-dots img p))

          ; finally, connect first and last too
          (define poly
            (render-line almost-connected (first p) (last p))))
    poly))