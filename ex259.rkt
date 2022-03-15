;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex259) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))

; A Word is one of:
; – '()
; – (cons 1String Word)
; interpretation: a Word is a list of 1Strings (letters)

(define de (list "d" "e"))
(define ed (list "e" "d"))
(define cat (list "c" "a" "t"))
(define act (list "a" "c" "t"))
(define rat (list "r" "a" "t"))
(define dear (list "d" "e" "a" "r"))

; A List-of-words is one of:
; — '()
; — (cons Word List-of-words)

(define de-ed (list de ed))
(define cat-act (list cat act))
(define cat-rat-dear (list cat rat dear))

; Word -> List-of-words
; creates all rearrangements of the letters in w

(check-expect (arrangements '()) (list '()))
(check-expect (arrangements (list "a")) (list (list "a")))
(check-expect (arrangements de) de-ed)

(define (arrangements w)
  (local
    (; 1String List-of-words -> List-of-words
     ; inserts x at the beginning, between all letters, and at the end
     ; of all words of l
     (define (insert-everywhere/in-all-words x l)
       (cond
         [(empty? l) '()]
         [else
          (append (insert-everywhere/in-one-word x (first l))
                  (insert-everywhere/in-all-words x (rest l)))]))

     ; 1String Word -> List-of-words
     ; inserts x at the beginning, between all letters, and at the end
     ; of w
     (define (insert-everywhere/in-one-word x w)
       (local
         (; 1String List-of-words -> List-of-words
          ; inserts x at the beginning of all words of l
          (define (insert-at-beginning/in-all-words x l)
            (cond
              [(empty? l) '()]
              [else
               (cons (insert-at-beginning/in-one-word x (first l))
                     (insert-at-beginning/in-all-words x (rest l)))]))

          ; 1String Word -> Word
          ; inserts x at the beginning of w
          (define (insert-at-beginning/in-one-word x w)
            (cons x w))

          ; result
          (define x-everywhere
            (cond
              [(empty? w) (list (list x))]
              [else
               (cons (insert-at-beginning/in-one-word x w)
                     (insert-at-beginning/in-all-words
                      (first w)
                      (insert-everywhere/in-one-word x (rest w))))])))
         x-everywhere))

     (define all-arrangements
       (cond
         [(empty? w) (list '())]
         [else (insert-everywhere/in-all-words
                (first w)
                (arrangements (rest w)))])))
    all-arrangements))