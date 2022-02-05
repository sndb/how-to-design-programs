;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex110) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Any BSL value is one of:
; – Number
; – Boolean
; – String
; – Image
; – (make-posn Any Any)
; ...
; – (make-tank Any Any)
; ...

#|
; Any -> ???
(define (checked-f v)
  (cond
    [(number? v) ...]
    [(boolean? v) ...]
    [(string? v) ...]
    [(image? v) ...]
    [(posn? v) (... (posn-x v) ... (posn-y v) ...)]
    ...
    [(tank? v) (... (tank-loc v) ... (tank-vel v) ...)]
    ...))
|#

; Any -> PositiveNumber
; computes the area of a disk with radius v,
; if v is a number
(define (checked-area-of-disk v)
  (cond
    [(and (real? v) (positive? v)) (area-of-disk v)]
    [else (error "area-of-disk: positive number expected")]))

; PositiveNumber -> PositiveNumber
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))