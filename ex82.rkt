;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex82) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Letter is one of:
; — 1String "a" through "z"
; — #false

; A TL-Word is a structure:
;   (make-tl-word Letter Letter Letter)
; interpretation: three-letter word
(define-struct tl-word [first second third])

; TL-Word TL-Word -> TL-Word
(check-expect (compare-word (make-tl-word "a" "b" "c")
                            (make-tl-word "a" "b" "c"))
              (make-tl-word "a" "b" "c"))
(check-expect (compare-word (make-tl-word "a" "b" "c")
                            (make-tl-word "a" "b" "d"))
              (make-tl-word "a" "b" #false))
(check-expect (compare-word (make-tl-word "x" "y" "z")
                            (make-tl-word "x" "w" "z"))
              (make-tl-word "x" #false "z"))
(check-expect (compare-word (make-tl-word "a" "b" "c")
                            (make-tl-word "x" "y" "z"))
              (make-tl-word #false #false #false))
(define (compare-word w1 w2)
  (make-tl-word (compare-letter (tl-word-first w1) (tl-word-first w2))
                (compare-letter (tl-word-second w1) (tl-word-second w2))
                (compare-letter (tl-word-third w1) (tl-word-third w2))))

; Letter Letter -> Letter
; yields l1 if l1 and l2 are equal, else #false
(check-expect (compare-letter "a" "b") #false)
(check-expect (compare-letter "a" "a") "a")
(define (compare-letter l1 l2)
  (if (equal? l1 l2) l1 #false))