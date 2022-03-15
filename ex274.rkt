;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex274) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] -> [List-of [List-of 1String]]
; produces the list of all prefixes of l

(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a" "b" "c"))
              (list
               (list "a" "b" "c")
               (list "a" "b")
               (list "a")))

(define (prefixes l)
  (local (; 1String [List-of [List-of 1String]]
          ; -> [List-of [List-of 1String]]
          (define (f x y)
            (if (empty? y)
                `((,x))
                (cons (reverse (cons x (reverse (first y)))) y))))
    (foldl f '() l)))

; [List-of 1String] -> [List-of [List-of 1String]]
; produces the list of all suffixes of l

(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b" "c"))
              (list
               (list "a" "b" "c")
               (list "b" "c")
               (list "c")))

(define (suffixes l)
  (local (; 1String [List-of [List-of 1String]]
          ; -> [List-of [List-of 1String]]
          (define (f x y)
            (if (empty? y)
                `((,x))
                (cons (cons x (first y)) y))))
    (foldr f '() l)))