;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex114-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

; Editor -> Image
; renders the text
(check-expect
 (render (make-editor "hello world" ""))
 (overlay/align "left" "center"
                (beside (text "hello world" TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "" TEXT-SIZE TEXT-COLOR))
                MTSCN))
(check-expect
 (render (make-editor "hello " "world"))
 (overlay/align "left" "center"
                (beside (text "hello " TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "world" TEXT-SIZE TEXT-COLOR))
                MTSCN))
(check-expect
 (render (make-editor "" "hello world"))
 (overlay/align "left" "center"
                (beside (text "" TEXT-SIZE TEXT-COLOR)
                        CURSOR
                        (text "hello world" TEXT-SIZE TEXT-COLOR))
                MTSCN))
(define (render e)
  (overlay/align "left" "center"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOR))
                 MTSCN))

; Editor KeyEvent -> Editor
; adds a single-character ke to the end of the pre field
; if ke is "\b", deletes the character to the left of the cursor
; if ke is "\t" or "\r", does nothing
; if ke is "left" or "right", moves the cursor accordingly
(check-expect (edit (make-editor "hello worl" "") "d")
              (make-editor "hello world" ""))
(check-expect (edit (make-editor "hello" "world") " ")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "" "ello world") "h")
              (make-editor "h" "ello world"))
(check-expect (edit (make-editor "hello world" "") "\b")
              (make-editor "hello worl" ""))
(check-expect (edit (make-editor "hello " "world") "\b")
              (make-editor "hello" "world"))
(check-expect (edit (make-editor "" "hello world") "\b")
              (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello " "world") "\t")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "\r")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "left")
              (make-editor "hello" " world"))
(check-expect (edit (make-editor "hello" " world") "right")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "" "hello world") "left")
              (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello world" "") "right")
              (make-editor "hello world" ""))
(check-expect (edit (make-editor "hello " "world") "down")
              (make-editor "hello " "world"))
(define (edit ed ke)
  (cond
    [(string=? "\b" ke) (erase ed)]
    [(or (string=? "\t" ke) (string=? "\r" ke)) ed]
    [(= (string-length ke) 1)
     (if (> (image-width (render (type ed ke))) WIDTH)
         ed
         (type ed ke))]
    [(string=? "left" ke) (move-left ed)]
    [(string=? "right" ke) (move-right ed)]
    [else ed]))

; Editor -> Editor
; removes the last character from pre field
(define (erase ed)
  (make-editor (string-remove-last (editor-pre ed))
               (editor-post ed)))

; Editor String -> Editor
; appends s to pre field
(define (type ed s)
  (make-editor (string-append (editor-pre ed) s)
               (editor-post ed)))

; Editor -> Editor
; moves cursor to the left
(define (move-left ed)
  (make-editor (string-remove-last (editor-pre ed))
               (string-append (string-last (editor-pre ed))
                              (editor-post ed))))

; Editor -> Editor
; moves cursor to the right
(define (move-right ed)
  (make-editor (string-append (editor-pre ed)
                              (string-first (editor-post ed)))
               (string-rest (editor-post ed))))

; String -> String
; removes the last character if any, else s
(check-expect (string-remove-last "") "")
(check-expect (string-remove-last "a") "")
(check-expect (string-remove-last "abc") "ab")
(define (string-remove-last s)
  (if (> (string-length s) 0)
      (substring s 0 (sub1 (string-length s)))
      s))

; String -> String
; yields the first character if any, else ""
(check-expect (string-first "") "")
(check-expect (string-first "a") "a")
(check-expect (string-first "abc") "a")
(define (string-first s)
  (if (> (string-length s) 0)
      (substring s 0 1)
      ""))

; String -> String
; yields the last character if any, else ""
(check-expect (string-last "") "")
(check-expect (string-last "a") "a")
(check-expect (string-last "abc") "c")
(define (string-last s)
  (if (> (string-length s) 0)
      (substring s (sub1 (string-length s)) (string-length s))
      ""))

; String -> String
; yields s with the first character removed if any, else ""
(check-expect (string-rest "") "")
(check-expect (string-rest "a") "")
(check-expect (string-rest "abc") "bc")
(define (string-rest s)
  (if (> (string-length s) 0)
      (substring s 1 (string-length s))
      ""))

; String -> Editor
; launches an interactive editor with preset pre
(define (run pre)
  (big-bang (make-editor pre "")
    [to-draw render]
    [on-key edit]
    [check-with editor?]))