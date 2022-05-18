;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex509) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right 

; [List-of 1String] N -> Editor
; produces (make-editor p s) such that
; (1) p and s make up ed
; (2) x is larger than the image of p and smaller than the image of p
; extended with the first 1String on s (if any)

(check-expect (split '() 24) (make-editor '() '()))
(check-expect (split (explode "hello") 0)
              (make-editor '() (explode "hello")))
(check-expect (split (explode "hello") 18)
              (make-editor (explode "lleh") '("o")))
(check-expect (split (explode "hello") 100)
              (make-editor (explode "olleh") '()))
(check-expect (split (explode "abcdef") 0)
              (make-editor '() (explode "abcdef")))
(check-expect (split (explode "abcdef") 7)
              (make-editor '("a") (explode "bcdef")))
(check-expect (split (explode "abcdef") 12)
              (make-editor (explode "ba") (explode "cdef")))
(check-expect (split (explode "abcdef") 33)
              (make-editor (explode "fedcba") '()))

(define (split ed x)
  (local (; [List-of 1String] [List-of 1String] -> Editor
          ; accumulator pre is a list of items that post lacks from l
          ; in reverse order
          (define (split/a pre post)
            (cond
              [(empty? post) (make-editor pre post)]
              [else
               (local ((define pre+1 (cons (first post) pre)))
                 (if (and (<= (image-width (editor-text pre)) x)
                          (< x (image-width (editor-text pre+1))))
                     (make-editor pre post)
                     (split/a pre+1 (rest post))))])))
    (split/a '() ed)))