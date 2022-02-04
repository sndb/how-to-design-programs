;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex87) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

(define-struct editor [text cursor])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation: (make-editor s t) describes an editor
; whose visible text is s with the cursor displayed
; after t characters

; Editor -> String
; yields the text before the cursor
(check-expect (editor-pre (make-editor "hello world" 5)) "hello")
(define (editor-pre ed)
  (substring (editor-text ed)
             0
             (editor-cursor ed)))

; Editor -> String
; yields the text after the cursor
(check-expect (editor-post (make-editor "hello world" 6)) "world")
(define (editor-post ed)
  (substring (editor-text ed)
             (editor-cursor ed)
             (string-length (editor-text ed))))

; Editor -> Image
; renders the text

(check-expect
 (render (make-editor "hello world" 11))
 (overlay/align "left" "center"
                (beside (text "hello world" TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "" TEXT-SIZE TEXT-COLOR))
                MTSCN))
(check-expect
 (render (make-editor "hello world" 6))
 (overlay/align "left" "center"
                (beside (text "hello " TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "world" TEXT-SIZE TEXT-COLOR))
                MTSCN))
(check-expect
 (render (make-editor "hello world" 0))
 (overlay/align "left" "center"
                (beside (text "" TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "hello world" TEXT-SIZE TEXT-COLOR))
                MTSCN))

(define (render ed)
  (overlay/align "left" "center"
                 (beside (text (editor-pre ed) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (editor-post ed) TEXT-SIZE TEXT-COLOR))
                 MTSCN))

; Editor KeyEvent -> Editor
; adds a single-character ke to the text field before the cursor
; if ke is "\b", deletes the character to the left of the cursor
; if ke is "\t" or "\r", does nothing
; if ke is "left" or "right", moves the cursor accordingly
(check-expect (edit (make-editor "hello worl" 10) "d")
              (make-editor "hello world" 11))
(check-expect (edit (make-editor "helloworld" 5) " ")
              (make-editor "hello world" 6))
(check-expect (edit (make-editor "ello world" 0) "h")
              (make-editor "hello world" 1))
(check-expect (edit (make-editor "hello world" 11) "\b")
              (make-editor "hello worl" 10))
(check-expect (edit (make-editor "hello world" 6) "\b")
              (make-editor "helloworld" 5))
(check-expect (edit (make-editor "hello world" 0) "\b")
              (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 6) "\t")
              (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "\r")
              (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "left")
              (make-editor "hello world" 5))
(check-expect (edit (make-editor "hello world" 5) "right")
              (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 0) "left")
              (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 11) "right")
              (make-editor "hello world" 11))
(check-expect (edit (make-editor "hello world" 6) "down")
              (make-editor "hello world" 6))
(define (edit ed ke)
  (cond
    [(string=? "\b" ke) (erase ed)]
    [(and (= (string-length ke) 1)
          (not (or
                (string=? "\t" ke)
                (string=? "\r" ke))))
     (if (> (image-width (render (type ed ke))) WIDTH)
         ed
         (type ed ke))]
    [(string=? "left" ke) (move-left ed)]
    [(string=? "right" ke) (move-right ed)]
    [else ed]))

; Editor -> Editor
; removes the character before the cursor
(define (erase ed)
  (make-editor (string-append (string-remove-last (editor-pre ed))
                              (editor-post ed))
               (if (> (editor-cursor ed) 0)
                   (sub1 (editor-cursor ed))
                   (editor-cursor ed))))

; Editor String -> Editor
; appends s before the cursor
(define (type ed s)
  (make-editor (string-append (editor-pre ed) s (editor-post ed))
               (+ (editor-cursor ed) (string-length s))))

; Editor -> Editor
; moves cursor to the left
(define (move-left ed)
  (make-editor (editor-text ed)
               (if (> (editor-cursor ed) 0 )
                   (sub1 (editor-cursor ed))
                   (editor-cursor ed))))

; Editor -> Editor
; moves cursor to the right
(define (move-right ed)
  (make-editor (editor-text ed)
               (if (< (editor-cursor ed) (string-length (editor-text ed)))
                   (add1 (editor-cursor ed))
                   (editor-cursor ed))))

; String -> String
; removes the last character if any, else s
(check-expect (string-remove-last "") "")
(check-expect (string-remove-last "a") "")
(check-expect (string-remove-last "abc") "ab")
(define (string-remove-last s)
  (if (> (string-length s) 0)
      (substring s 0 (sub1 (string-length s)))
      s))

; String -> Editor
; launches an interactive editor with preset pre
(define (run pre)
  (big-bang (make-editor pre (string-length pre))
    [to-draw render]
    [on-key edit]))