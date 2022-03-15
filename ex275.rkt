;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex275) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a [List-of String].
(define AS-LIST (read-lines LOCATION))
(define EX-DICT '("apple"
                  "animal"
                  "alpha"
                  "beta"
                  "gamma"
                  "delta"
                  "epsilon"))

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

(define-struct lc [letter count])
; A Letter-Count is a structure:
;   (make-lc Letter Number)
; interpretation: (make-lc l c) combines the letter l with
; the count c

; Letter Dictionary -> Number
; counts how many words in d start with l

(check-expect (starts-with# "a" EX-DICT) 3)
(check-expect (starts-with# "b" EX-DICT) 1)
(check-expect (starts-with# "z" EX-DICT) 0)

(define (starts-with# l d)
  (local (; String Number -> Number
          (define (inc x y)
            (if (string=? l (string-ith x 0))
                (add1 y)
                y)))
    (foldr inc 0 d)))

; Dictionary [List-of Letter] -> [List-of Letter-Count]
; reports how often the letters l occur as first ones in d

(check-expect (count-by-letter EX-DICT
                               '("a" "b" "z"))
              `(,(make-lc "a" 3)
                ,(make-lc "b" 1)
                ,(make-lc "z" 0)))

(define (count-by-letter d l)
  (local (; Letter -> Letter-Count
          (define (how-often t)
            (make-lc t (starts-with# t d))))
    (map how-often l)))

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that occurs most
; often as the first one in the given non-empty Dictionary

(check-expect (most-frequent '("hello")) (make-lc "h" 1))
(check-expect (most-frequent EX-DICT) (make-lc "a" 3))

(define (most-frequent d)
  (local (; 1. count letters
          (define lcs (count-by-letter d LETTERS))
          ; 2. sort the letter counts
          (define sorted (sort lcs lc>=?))
          ; 3. pick the most frequent lc
          (define most (first sorted)))
    most))

; Letter-Count Letter-Count -> Boolean
; compares the counts of lc1 and lc2 with >=

(check-expect (lc>=? (make-lc "b" 1) (make-lc "a" 3))
              #false)
(check-expect (lc>=? (make-lc "b" 1) (make-lc "z" 0))
              #true)

(define (lc>=? lc1 lc2)
  (>= (lc-count lc1) (lc-count lc2)))

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that occurs most
; often as the first one in the given non-empty Dictionary

(check-expect (most-frequent.v2 '("hello")) (make-lc "h" 1))
(check-expect (most-frequent.v2 EX-DICT) (make-lc "a" 3))

(define (most-frequent.v2 d)
  (local (; 1. get dictionaries
          (define dicts (words-by-first-letter d))
          ; 2. convert to letter counts
          (define lcs (dicts->lcs dicts))
          ; 3. sort the letter counts
          (define sorted (sort lcs lc>=?))
          ; 4. pick the most frequent lc
          (define most (first sorted)))
    most))

; [List-of Dictionary] -> [List-of Letter-Count]
; produces the list of letter counts for each dictionary

(check-expect (dicts->lcs '()) '())
(check-expect (dicts->lcs '(("alpha")))
              `(,(make-lc "a" 1)))
(check-expect (dicts->lcs '(("alpha")
                            ("beta")))
              `(,(make-lc "a" 1)
                ,(make-lc "b" 1)))
(check-expect (dicts->lcs '(("apple" "animal" "alpha")
                            ("beta")
                            ("gamma")
                            ("delta")
                            ("epsilon")))
              `(,(make-lc "a" 3)
                ,(make-lc "b" 1)
                ,(make-lc "g" 1)
                ,(make-lc "d" 1)
                ,(make-lc "e" 1)))

(define (dicts->lcs l)
  (local (; Dictionary -> Letter-Count
          (define (f d)
            (make-lc (first-word-letter d)
                     (length d))))
    (map f l)))

; Dictionary -> [List-of Dictionary]
; produces a list of dictionaries, one per letter

(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter '("alpha"))
              '(("alpha")))
(check-expect (words-by-first-letter '("alpha" "beta"))
              '(("alpha")
                ("beta")))
(check-expect (words-by-first-letter EX-DICT)
              '(("apple" "animal" "alpha")
                ("beta")
                ("delta")
                ("epsilon")
                ("gamma")))

(define (words-by-first-letter d)
  (remove-empty (words-by-first-letters d LETTERS)))

; [List-of Dictionary] -> [List-of Dictionary]
; removes empty dictionaries from l

(check-expect (remove-empty '()) '())
(check-expect (remove-empty '(("a") ("b" "c") ()))
              '(("a") ("b" "c")))

(define (remove-empty l)
  (local (; Dictionary -> Boolean
          (define (not-empty? d)
            (not (empty? d))))
    (filter not-empty? l)))

; Dictionary [List-of Letter] -> [List-of Dictionary]
; produces a list of dictionaries, one per letter in l

(check-expect (words-by-first-letters '() '()) '())
(check-expect (words-by-first-letters '("alpha")
                                      '("a"))
              '(("alpha")))
(check-expect (words-by-first-letters '("alpha" "beta")
                                      '("a" "b"))
              '(("alpha")
                ("beta")))
(check-expect (words-by-first-letters EX-DICT
                                      '("a" "b" "g"))
              '(("apple" "animal" "alpha")
                ("beta")
                ("gamma")))

(define (words-by-first-letters d l)
  (local (; Letter -> Dictionary
          (define (f t)
            (starts-with t d)))
    (map f l)))

; Letter Dictionary -> Dictionary
; produces a dictionary containing words starting with l

(check-expect (starts-with "a" '()) '())
(check-expect (starts-with "a" '("alpha"))
              '("alpha"))
(check-expect (starts-with "b" '("alpha" "beta"))
              '("beta"))
(check-expect (starts-with "a" EX-DICT)
              '("apple" "animal" "alpha"))

(define (starts-with l d)
  (local (; String -> Boolean
          (define (f w)
            (string=? l (string-ith w 0))))
    (filter f d)))

; Dictionary -> 1String
; produces the first letter of the first word in non-empty d

(check-expect (first-word-letter '("alpha")) "a")
(check-expect (first-word-letter '("beta" "gamma")) "b")

(define (first-word-letter d)
  (string-ith (first d) 0))