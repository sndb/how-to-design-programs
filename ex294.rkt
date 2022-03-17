;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex294) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise

(check-satisfied (index 3 '(1 2 3 4 5))
                 (is-index? 3 '(1 2 3 4 5)))
(check-satisfied (index 0 '(1 2 3 4 5))
                 (is-index? 0 '(1 2 3 4 5)))

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; [X] X [List-of X] -> [[Maybe N] -> Boolean]
; is i the index of the first occurrence of x in l, #false otherwise

(check-expect [(is-index? 3 '(1 2 3 4 5)) 2] #true)
(check-expect [(is-index? 0 '(1 2 3 4 5)) #false] #true)
(check-expect [(is-index? 3 '(1 2 3 4 5 3 4)) 2] #true)
(check-expect [(is-index? 3 '(1 2 3 4 5 3 4)) 5] #false)
(check-expect [(is-index? 3 '(1 2 3 4 5)) 1] #false)
(check-expect [(is-index? 0 '(1 2 3 4 5)) 1] #false)
(check-expect [(is-index? 3 '(1 2 3 4 5)) 10] #false)

(define (is-index? x l)
  (lambda (i)
    (cond
      [(false? i) (not (member? x l))]
      [else
       (local (; [X] N [List-of X] -> [List-of X]
               ; yields first i items from l
               (define (slice l i)
                 (build-list i (lambda (n) (list-ref l n)))))
         (and (< i (length l))
              (not (member? x (slice l i)))
              (equal? (list-ref l i) x)))])))