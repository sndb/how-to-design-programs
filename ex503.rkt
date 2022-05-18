;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex503) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 

(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate '((0 4 5) (0 6 7) (1 2 3)))
              '((1 2 3) (0 4 5) (0 6 7)))
(check-error (rotate '((0 4 5) (0 2 3)))
             "all rows start with 0")

(define (rotate M)
  (local (; Matrix -> Matrix
          (define (rotate-helper M)
            (cond
              [(not (= (first (first M)) 0)) M]
              [else
               (rotate (append (rest M) (list (first M))))])))
    (if (andmap (lambda (row) (= (first row) 0)) M)
        (error "all rows start with 0")
        (rotate-helper M))))

(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate.v2 '((0 4 5) (0 6 7) (1 2 3)))
              '((1 2 3) (0 4 5) (0 6 7)))
(check-error (rotate.v2 '((0 4 5) (0 2 3)))
             "all rows start with 0")

(define (rotate.v2 M0)
  (local (; Matrix Matrix -> Matrix 
          ; accumulator seen is the list of rows that M lacks from M0
          (define (rotate/a M seen)
            (cond
              [(empty? M)
               (error "all rows start with 0")]
              [(not (= (first (first M)) 0))
               (append M (reverse seen))]
              [else
               (rotate/a (rest M) (cons (first M) seen))])))
    (rotate/a M0 '())))

(define M
  (reverse (build-list 5000 (lambda (n)
                              (if (zero? n) '(1 1 1) '(0 1 1))))))
(time (first (rotate M)))
(time (first (rotate.v2 M)))