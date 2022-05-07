;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex431) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Answers for the key questions for quick-sort<
;
; Q: What is a trivially solvable problem?
; A: (empty? alon)
; Q: How are trivial problems solved?
; A: '()
; Q: How does the algorithm generate new problems that are more easily
; solvable than the original one? Is there one new problem that we
; generate or are there several?
; A: (smallers alon pivot), (largers alon pivot)
; Q: Is the solution of the given problem the same as the solution of
; (one of) the new problems? Or, do we need to combine the solutions to
; create a solution for the original problem? And, if so, do we need
; anything from the original problem data?
; A: (append (quick-sort ...) (list pivot) (quick-sort ...))
;
; 2 instances of generate-problem are needed.

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

;; Answers for the key questions for bundle
;
; Q: What is a trivially solvable problem?
; A: (empty? s)
; Q: How are trivial problems solved?
; A: '()
; Q: How does the algorithm generate new problems that are more easily
; solvable than the original one? Is there one new problem that we
; generate or are there several?
; A: (drop s n)
; Q: Is the solution of the given problem the same as the solution of
; (one of) the new problems? Or, do we need to combine the solutions to
; create a solution for the original problem? And, if so, do we need
; anything from the original problem data?
; A: (cons (implode (take s n)) (bundle ...))
;
; 1 instance of generate-problem is needed.

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))