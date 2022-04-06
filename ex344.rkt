;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex344) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; A File is a structure: 
;   (make-file String N String)

; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

; A Path is [List-of String].
; interpretation directions into a directory tree

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

; Dir -> [List-of Path]
; lists the paths to all files contained in d

(check-expect (ls-R code-dir)
              '(("Code" "hang")
                ("Code" "draw")))
(check-expect (ls-R libs-dir)
              '(("Libs" "Code" "hang")
                ("Libs" "Code" "draw")
                ("Libs" "Docs" "read!")))

(define (ls-R d)
  (map (lambda (p) (cons (dir-name d) p))
       (append (foldr append '()
                      (map ls-R
                           (dir-dirs d)))
               (foldr append '()
                      (map (lambda (f) (list (list (file-name f))))
                           (dir-files d))))))

; Dir String -> [List-of Path]
; produces the list of all paths that lead to n in d

(check-expect (find-all libs-dir "part1") '())
(check-expect (find-all figure123 "part4") '())
(check-expect (find-all figure123 "part1")
              '(("TS" "Text" "part1")))
(check-expect (find-all figure123 "hang")
              '(("TS" "Libs" "Code" "hang")))
(check-member-of (find-all figure123 "read!")
                 '(("TS" "read!")
                   ("TS" "Libs" "Docs" "read!"))
                 '(("TS" "Libs" "Docs" "read!")
                   ("TS" "read!")))

(define (find-all d n)
  (filter (lambda (p) (string=? n (first (reverse p)))) (ls-R d)))