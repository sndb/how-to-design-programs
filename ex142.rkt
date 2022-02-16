;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex142) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A List-of-images is one of:
; — '()
; — (cons Image List-of-images)

; ImageOrFalse is one of:
; — Image
; — #false

; List-of-images PositiveNumber -> ImageOrFalse
; produces the first image on loi that is not an n by n square;
; if the function cannot find such an image, it produces #false

(check-expect (ill-sized? '() 20) #false)
(check-expect
 (ill-sized? (cons (square 20 "solid" "red")
                   (cons (circle 10 "solid" "green") '()))
             20)
 #false)
(check-expect
 (ill-sized? (cons (square 20 "solid" "red")
                   (cons (ellipse 20 10 "solid" "green") '()))
             20)
 (ellipse 20 10 "solid" "green"))

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else
     (if (not (square? (first loi) n))
         (first loi)
         (ill-sized? (rest loi) n))]))

; Image PositiveNumber -> Boolean
; #true if i is an n by n square, else #false

(check-expect (square? (square 20 "solid" "red") 20) #true)
(check-expect (square? (square 12 "solid" "red") 20) #false)
(check-expect (square? (ellipse 20 10 "solid" "green") 20) #false)

(define (square? i n)
  (and (= n (image-height i))
       (= n (image-width i))))