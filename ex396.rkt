;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex396) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; run the game with (play (list-ref AS-LIST (random SIZE)) 10)

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word Letter -> HM-Word
; produces s with all "_" where the guess g revealed a letter
; assume: w and s are of same length

(check-expect (compare-word '() '() "a") '())

(check-expect (compare-word (explode "hello")
                            (explode "h___o")
                            "a")
              (explode "h___o"))

(check-expect (compare-word (explode "hello")
                            (explode "h___o")
                            "e")
              (explode "he__o"))

(check-expect (compare-word (explode "hello")
                            (explode "he__o")
                            "l")
              (explode "hello"))

(define (compare-word w s g)
  (cond
    [(empty? w) '()]
    [(cons? w)
     (if (string=? g (first w))
         (cons g (compare-word (rest w) (rest s) g))
         (cons (first s) (compare-word (rest w) (rest s) g)))]))