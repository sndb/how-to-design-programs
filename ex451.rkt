;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex451) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))
 
; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= 0")))
 
(define table2 (make-table 1 a2))

(define table3 (make-table 10 (lambda (i) (- i 5))))

(define table4 (make-table 4 (lambda (i) (cond
                                           [(= i 0) -7]
                                           [(= i 1) -4]
                                           [(= i 2) -1]
                                           [(= i 3) 1]))))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> N
; finds the smallest index for a root of the table
; assume: t is monotonically increasing

(check-expect (find-linear table1) 0)
(check-error (find-linear table2) "table do not have a root")
(check-expect (find-linear table3) 5)
(check-error (find-linear table4) "table do not have a root")

(define (find-linear t)
  (local (; N -> N
          (define (find-linear-helper i)
            (cond
              [(= i (table-length t))
               (error "table do not have a root")]
              [else
               (if (<= (abs (table-ref t i)) ε)
                   i
                   (find-linear-helper (add1 i)))])))
    (find-linear-helper 0)))

; Table -> N
; finds the smallest index for a root of the table
; assume: t is monotonically increasing
; generative: divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination: cuts interval in half with each iteration, arriving
; either at root or at error case

(check-expect (find-binary table1) 0)
(check-error (find-binary table2) "table do not have a root")
(check-expect (find-binary table3) 5)
(check-error (find-binary table4) "table do not have a root")

(define (find-binary t)
  (local (; N N -> N
          (define (find-binary-helper i j)
            (local ((define r@i (table-ref t i))
                    (define r@j (table-ref t j))
                    (define mid (quotient (+ i j) 2))
                    (define r@mid (table-ref t mid)))
              (cond
                [(<= (abs r@mid) ε) mid]
                [(<= r@i 0 r@mid) (find-binary-helper i (sub1 mid))]
                [(<= r@mid 0 r@j) (find-binary-helper (add1 mid) j)]
                [else (error "table do not have a root")]))))
    (find-binary-helper 0 (sub1 (table-length t)))))