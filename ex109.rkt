;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 100)
(define HEIGHT 100)

(define WHITE (empty-scene WIDTH HEIGHT "white"))
(define YELLOW (empty-scene WIDTH HEIGHT "yellow"))
(define GREEN (empty-scene WIDTH HEIGHT "green"))
(define RED (empty-scene WIDTH HEIGHT "red"))

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")
; ExpectsToSee is one of:
; – AA
; – BB
; – DD
; – ER

; ExpectsToSee -> Image
; displays the rectangle corresponding to the current state
(check-expect (display-rectangle AA) WHITE)
(check-expect (display-rectangle BB) YELLOW)
(check-expect (display-rectangle DD) GREEN)
(check-expect (display-rectangle ER) RED)
(define (display-rectangle s)
  (cond
    [(string=? AA s) WHITE]
    [(string=? BB s) YELLOW]
    [(string=? DD s) GREEN]
    [(string=? ER s) RED]))

; ExpectsToSee KeyEvent -> ExpectsToSee
; updates the current state on user input
; looks for sequences that start with "a", followed by an arbitrarily long mix
; of "b" and "c", and ended by a "d"
(check-expect (handle-input AA "a") BB)
(check-expect (handle-input AA "b") ER)
(check-expect (handle-input BB "b") BB)
(check-expect (handle-input BB "c") BB)
(check-expect (handle-input BB "a") ER)
(check-expect (handle-input BB "d") DD)
(check-expect (handle-input DD "b") DD)
(check-expect (handle-input DD "d") DD)
(check-expect (handle-input ER "a") ER)
(check-expect (handle-input ER "b") ER)
(check-expect (handle-input ER "d") ER)
(define (handle-input s ke)
  (cond
    [(string=? AA s)
     (cond
       [(string=? "a" ke) BB]
       [else ER])]
    [(string=? BB s)
     (cond
       [(or (string=? "b" ke)
            (string=? "c" ke))
        BB]
       [(string=? "d" ke) DD]
       [else ER])]
    [(string=? DD s) s]
    [(string=? ER s) s]))

; ExpectsToSee -> ExpectsToSee
; runs the program
(define (main s)
  (big-bang s
    [to-draw display-rectangle]
    [on-key handle-input]))