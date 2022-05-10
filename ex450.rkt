;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex450) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous and monotonically increasing
; assume (<= (f left) 0 (f right))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination each function call cuts the interval in half towards 0,
; terminates when the threshold is small enough

(check-satisfied (find-root simple 0 40) (check-fun simple))

(define (find-root f left right)
  (local (; [Number -> Number] Number Number Number Number -> Number
          (define (find-root-helper f left right f@left f@right)
            (cond
              [(<= (- right left) ε) left]
              [else
               (local ((define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(<= f@left 0 f@mid)
                    (find-root-helper f left mid f@left f@mid)]
                   [(<= f@mid 0 f@right)
                    (find-root-helper f mid right f@mid f@right)]))])))
    (find-root-helper f left right (f left) (f right))))

; Number -> Number
(define (simple x)
  (- x 24))

; Number -> Boolean
(define (check-fun f)
  (lambda (x) (<= (abs (f x)) ε)))