;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise

(check-satisfied (find 2 '(1 2 3 4 5))
                 (found? 2 '(1 2 3 4 5)))
(check-satisfied (find 0 '(1 2 3 4 5))
                 (found? 0 '(1 2 3 4 5)))

(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; [X] X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
; is l the first sublist of k that starts with x, #false otherwise

(check-expect [(found? 2 '(1 2 3)) '(2 3)] #true)
(check-expect [(found? 4 '(1 2 3)) #false] #true)
(check-expect [(found? 2 '(1 2 3 2 3)) '(2 3 2 3)] #true)
(check-expect [(found? 2 '(1 2 3 2 3)) '(2 3)] #false)

(define (found? x k)
  (lambda (l)
    (cond
      [(false? l) (not (member? x k))]
      [else (and (not (empty? l))
                 (first-sublist? x l k))])))

; [X] X [NEList-of X] [List-of X] -> Boolean
; is l the first sublist of k that starts with x

(check-expect (first-sublist? 2 '(2 3 2 3) '(1 2 3 2 3)) #true)
(check-expect (first-sublist? 2 '(2 3) '(1 2 3 2 3)) #false)

(define (first-sublist? x l k)
  (cond
    [(empty? k) #false]
    [else
     (if (equal? x (first k))
         (equal? l k)
         (first-sublist? x l (rest k)))]))