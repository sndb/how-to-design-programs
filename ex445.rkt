;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex445) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.01)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in 
; one of the two halves, picks according to (2)

(check-satisfied (find-root poly -3 3)
                 (lambda (x) (<= (abs (poly x)) ε)))

(define (find-root f left right)
  0)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

;step , left    , f left      , right , f right  , mid     , f mid
;n=1  , 3       , -1          , 6.00  , 8.00     , 4.50    , 1.25
;n=2  , 3       , -1          , 4.50  , 1.25     , 3.75    , -0.4375
;n=3  , 3.75    , -0.4375     , 4.50  , 1.25     , 4.125   , 0.265625
;n=4  , 3.75    , -0.4375     , 4.125 , 0.26562 5, 3.9375  , -0.12109375
;n=5  , 3.9375  , -0.12109375 , 4.125 , 0.265625 , 4.03125 , 0.0634765625
;n=6  , 3.9375  , -0.12109375 , 4.03125,0.0634765625,3.984375,-0.031005859375
;n=7  , 3.984375,-0.031005859375,4.03125,0.0634765625,4.0078125,0.01568603515625