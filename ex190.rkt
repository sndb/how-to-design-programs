;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

; An LLo1S is one of:
; — '()
; — (cons Lo1S LLo1S)

; Lo1S -> LLo1S
; produces the list of all prefixes of l

(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a" "b" "c"))
              (list
               (list "a" "b" "c")
               (list "a" "b")
               (list "a")))

(define (prefixes l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons l (prefixes (remove-last l)))]))

; Lo1S -> Lo1S
; removes the last element of l

(check-expect (remove-last (list "a"))
              '())
(check-expect (remove-last (list "a" "b" "c"))
              (list "a" "b"))

(define (remove-last l)
  (reverse (rest (reverse l))))

; Lo1S -> LLo1S
; produces the list of all suffixes of l

(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b" "c"))
              (list
               (list "a" "b" "c")
               (list "b" "c")
               (list "c")))

(define (suffixes l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons l (suffixes (rest l)))]))