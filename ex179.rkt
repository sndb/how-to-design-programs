;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex179) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; data example 1:
(make-editor all good)

; data example 2:
(make-editor lla good)

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render e) MT)

; Editor KeyEvent -> Editor
; deals with a key event, given some editor

(check-expect
 (editor-kh (create-editor "" "") "e")
 (create-editor "e" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "e")
 (create-editor "cde" "fgh"))
(check-expect
 (editor-kh (create-editor "" "") "\b")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cde" "fgh") "\b")
 (create-editor "cd" "fgh"))
(check-expect
 (editor-kh (create-editor "" "cdefgh") "\b")
 (create-editor "" "cdefgh"))
(check-expect
 (editor-kh (create-editor "cdefgh" "") "\b")
 (create-editor "cdefg" ""))
(check-expect
 (editor-kh (create-editor "" "") "left")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cde" "fgh") "left")
 (create-editor "cd" "efgh"))
(check-expect
 (editor-kh (create-editor "" "cdefgh") "left")
 (create-editor "" "cdefgh"))
(check-expect
 (editor-kh (create-editor "cdefgh" "") "left")
 (create-editor "cdefg" "h"))
(check-expect
 (editor-kh (create-editor "" "") "right")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cde" "fgh") "right")
 (create-editor "cdef" "gh"))
(check-expect
 (editor-kh (create-editor "" "cdefgh") "right")
 (create-editor "c" "defgh"))
(check-expect
 (editor-kh (create-editor "cdefgh" "") "right")
 (create-editor "cdefgh" ""))
(check-expect
 (editor-kh (create-editor "" "") "up")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cde" "fgh") "up")
 (create-editor "cde" "fgh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible

(check-expect
 (editor-lft (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-lft (make-editor (cons "d" (cons "c" '()))
                          (cons "e" (cons "f" '()))))
 (make-editor (cons "c" '())
              (cons "d" (cons "e" (cons "f" '())))))
(check-expect
 (editor-lft (make-editor '()
                          (cons "e" (cons "f" '()))))
 (make-editor '()
              (cons "e" (cons "f" '()))))
(check-expect
 (editor-lft (make-editor (cons "d" (cons "c" '()))
                          '()))
 (make-editor (cons "c" '())
              (cons "d" '())))

(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed))
                         (editor-post ed)))))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible

(check-expect
 (editor-rgt (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-rgt (make-editor (cons "d" (cons "c" '()))
                          (cons "e" (cons "f" '()))))
 (make-editor (cons "e" (cons "d" (cons "c" '())))
              (cons "f" '())))
(check-expect
 (editor-rgt (make-editor '()
                          (cons "e" (cons "f" '()))))
 (make-editor (cons "e" '())
              (cons "f" '())))
(check-expect
 (editor-rgt (make-editor (cons "d" (cons "c" '()))
                          '()))
 (make-editor (cons "d" (cons "c" '()))
              '()))

(define (editor-rgt ed)
  (if (empty? (editor-post ed))
      ed
      (make-editor (cons (first (editor-post ed))
                         (editor-pre ed))
                   (rest (editor-post ed)))))

; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible

(check-expect
 (editor-del (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-del (make-editor (cons "d" (cons "c" '()))
                          (cons "e" (cons "f" '()))))
 (make-editor (cons "c" '())
              (cons "e" (cons "f" '()))))
(check-expect
 (editor-del (make-editor '()
                          (cons "e" (cons "f" '()))))
 (make-editor '()
              (cons "e" (cons "f" '()))))
(check-expect
 (editor-del (make-editor (cons "d" (cons "c" '()))
                          '()))
 (make-editor (cons "c" '())
              '()))

(define (editor-del ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed)) (editor-post ed))))

; Editor 1String -> Editor
; insert the 1String k between pre and post

(check-expect
 (editor-ins (make-editor '() '()) "e")
 (make-editor (cons "e" '()) '()))
 
(check-expect
 (editor-ins
  (make-editor (cons "d" '())
               (cons "f" (cons "g" '())))
  "e")
 (make-editor (cons "e" (cons "d" '()))
              (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect
 (rev (cons "a" (cons "b" (cons "c" '()))))
 (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l

(check-expect
 (add-at-end '() "a")
 (cons "a" '()))
(check-expect
 (add-at-end (cons "c" (cons "b" '())) "a")
 (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

; String String -> Editor

(check-expect
 (create-editor "all" "good")
 (make-editor lla good))

(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))