;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex432) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define HSEGMENTS 10)
(define VSEGMENTS 10)

; Posn -> Posn
; produces a randomly chosen Posn distinct from p

(check-satisfied
 (food-create (make-posn 1 1))
 (lambda (p) (not (and (= (posn-x p) 1) (= (posn-y p) 1)))))

(define (food-create p)
  (local ((define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate)))
    (food-check-create p (make-posn (random HSEGMENTS)
                                    (random VSEGMENTS)))))