;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex337) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File])

(define code-dir
  (make-dir.v3 "Code"
               '()
               `(,(make-file "hang" 8 "")
                 ,(make-file "draw" 2 ""))))

(define docs-dir
  (make-dir.v3 "Docs"
               '()
               `(,(make-file "read!" 19 ""))))

(define text-dir
  (make-dir.v3 "Text"
               '()
               `(,(make-file "part1" 99 "")
                 ,(make-file "part2" 52 "")
                 ,(make-file "part3" 17 ""))))

(define libs-dir
  (make-dir.v3 "Libs"
               `(,code-dir ,docs-dir)
               '()))

(define figure123
  (make-dir.v3 "TS"
               `(,text-dir ,libs-dir)
               `(,(make-file "read!" 10 ""))))

; Dir.v3 -> N
; determines how many files d contains

(check-expect (how-many (make-dir.v3 "a" '() '())) 0)
(check-expect (how-many figure123) 7)

(define (how-many d)
  (foldr + (length (dir.v3-files d)) (map how-many (dir.v3-dirs d))))