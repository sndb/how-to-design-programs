;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex340) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; A File is a structure: 
;   (make-file String N String)

; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

(define code-dir
  (make-dir "Code"
            '()
            `(,(make-file "hang" 8 "")
              ,(make-file "draw" 2 ""))))

(define docs-dir
  (make-dir "Docs"
            '()
            `(,(make-file "read!" 19 ""))))

(define text-dir
  (make-dir "Text"
            '()
            `(,(make-file "part1" 99 "")
              ,(make-file "part2" 52 "")
              ,(make-file "part3" 17 ""))))

(define libs-dir
  (make-dir "Libs"
            `(,code-dir ,docs-dir)
            '()))

(define figure123
  (make-dir "TS"
            `(,text-dir ,libs-dir)
            `(,(make-file "read!" 10 ""))))

; Dir -> [List-of String]
; lists the names of all files and directories in a given Dir

(check-expect (ls code-dir) '("hang" "draw"))
(check-expect (ls figure123) '("Text" "Libs" "read!"))

(define (ls d)
  (append (map dir-name (dir-dirs d))
          (map file-name (dir-files d))))