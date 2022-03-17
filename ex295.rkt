;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex295) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [List-of Posn]
; generates n non-random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
(define (random-posns/bad n)
  (build-list n (lambda (x) (make-posn 0 0))))

; N -> [[List-of Posn] -> Boolean]
; is length of l is n
; all Posns in l are within a WIDTH by HEIGHT rectangle

(check-expect [(n-inside-playground? 3)
               `(,(make-posn 0 0)
                 ,(make-posn 0 299)
                 ,(make-posn 299 0))]
              #true)
(check-expect [(n-inside-playground? 3)
               `(,(make-posn 0 0)
                 ,(make-posn 150 150)
                 ,(make-posn 299 299))]
              #true)
(check-expect [(n-inside-playground? 3)
               `(,(make-posn -1 0)
                 ,(make-posn 150 150)
                 ,(make-posn 299 299))]
              #false)
(check-expect [(n-inside-playground? 3)
               `(,(make-posn 0 -1)
                 ,(make-posn 150 150)
                 ,(make-posn 299 299))]
              #false)
(check-expect [(n-inside-playground? 3)
               `(,(make-posn 0 0)
                 ,(make-posn 0 300)
                 ,(make-posn 299 0))]
              #false)
(check-expect [(n-inside-playground? 3)
               `(,(make-posn 0 0)
                 ,(make-posn 0 299)
                 ,(make-posn 300 0))]
              #false)

(define (n-inside-playground? n)
  (lambda (l)
    (and (= (length l) n)
         (andmap
          (lambda (p)
            (and (and (<= 0 (posn-x p)) (< (posn-x p) WIDTH))
                 (and (<= 0 (posn-y p)) (< (posn-y p) HEIGHT))))
          l))))