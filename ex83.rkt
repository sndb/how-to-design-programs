;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex83) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define MTSCN (empty-scene 200 20))
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