;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex449) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination each function call cuts the interval in half towards 0,
; terminates when the threshold is small enough

(check-satisfied (find-root poly -3 3) check-poly)
(check-satisfied (find-root poly 3 10) check-poly)

(define (find-root f left right)
  (local (; [Number -> Number] Number Number Number Number -> Number
          (define (find-root-helper f left right f@left f@right)
            (cond
              [(<= (- right left) ε) left]
              [else
               (local ((define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
                    (find-root-helper f left mid f@left f@mid)]
                   [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
                    (find-root-helper f mid right f@mid f@right)]))])))
    (find-root-helper f left right (f left) (f right))))

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number -> Boolean
(define (check-poly x)
  (<= (abs (poly x)) ε))