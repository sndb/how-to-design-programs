;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex213) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w) (member? "art" w) (member? "tar" w)))

; String -> List-of-strings
; finds all words that the letters of some given word spell

(check-member-of (alternative-words "cat")
                 (list "cat" "act")
                 (list "act" "cat"))

(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; List-of-words -> List-of-strings
; turns all Words in low into Strings

(check-expect (words->strings '()) '())
(check-expect (words->strings (list cat))
              (list "cat"))
(check-expect (words->strings (list cat rat))
              (list "cat" "rat"))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (word->string (first low)) (words->strings (rest low)))]))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary

(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "cat" "rat"))
              (list "cat" "rat"))
(check-expect (in-dictionary (list "cat" "rat" "jklzxcvuiop"))
              (list "cat" "rat"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (member? (first los) DICTIONARY)
         (cons (first los) (in-dictionary (rest los)))
         (in-dictionary (rest los)))]))

; Word -> List-of-words
; creates all rearrangements of the letters in w

(check-expect (arrangements '()) (list '()))
(check-expect (arrangements (list "a")) (list (list "a")))
(check-expect (arrangements de) de-ed)

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words
           (first w)
           (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
; inserts x at the beginning, between all letters, and at the end of
; all words of l

(check-expect (insert-everywhere/in-all-words "a" '()) '())

(check-expect
 (insert-everywhere/in-all-words "a"
                                 (list (list "b")))
 (list (list "a" "b")
       (list "b" "a")))

(check-expect
 (insert-everywhere/in-all-words "a"
                                 (list (list "b")
                                       (list "c" "d")))
 (list (list "a" "b")
       (list "b" "a")
       (list "a" "c" "d")
       (list "c" "a" "d")
       (list "c" "d" "a")))

(define (insert-everywhere/in-all-words x l)
  (cond
    [(empty? l) '()]
    [else
     (append (insert-everywhere/in-one-word x (first l))
             (insert-everywhere/in-all-words x (rest l)))]))

; 1String Word -> List-of-words
; inserts x at the beginning, between all letters, and at the end of w

(check-expect (insert-everywhere/in-one-word "a" '())
              (list (list "a")))

(check-expect (insert-everywhere/in-one-word "a" (list "b"))
              (list (list "a" "b")
                    (list "b" "a")))

(check-expect (insert-everywhere/in-one-word "a" (list "c" "d"))
              (list (list "a" "c" "d")
                    (list "c" "a" "d")
                    (list "c" "d" "a")))

(define (insert-everywhere/in-one-word x w)
  (cond
    [(empty? w) (list (list x))]
    [else
     (cons (insert-at-beginning/in-one-word x w)
           (insert-at-beginning/in-all-words
            (first w)
            (insert-everywhere/in-one-word x (rest w))))]))

; 1String List-of-words -> List-of-words
; inserts x at the beginning of all words of l

(check-expect (insert-at-beginning/in-all-words "a" '()) '())

(check-expect
 (insert-at-beginning/in-all-words "a" (list (list "b")))
 (list (list "a" "b")))

(check-expect
 (insert-at-beginning/in-all-words "a" (list (list "b")
                                             (list "c" "d")))
 (list (list "a" "b")
       (list "a" "c" "d")))

(define (insert-at-beginning/in-all-words x l)
  (cond
    [(empty? l) '()]
    [else
     (cons (insert-at-beginning/in-one-word x (first l))
           (insert-at-beginning/in-all-words x (rest l)))]))

; 1String Word -> Word
; inserts x at the beginning of w

(check-expect (insert-at-beginning/in-one-word "a" '())
              (list "a"))
(check-expect (insert-at-beginning/in-one-word "a" (list "b"))
              (list "a" "b"))
(check-expect (insert-at-beginning/in-one-word "a" (list "c" "d"))
              (list "a" "c" "d"))

(define (insert-at-beginning/in-one-word x w)
  (cons x w))

; String -> Word
; converts s to the chosen word representation

(check-expect (string->word "") '())
(check-expect (string->word "cat") cat)

(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string

(check-expect (word->string '()) "")
(check-expect (word->string cat) "cat")

(define (word->string w)
  (implode w))