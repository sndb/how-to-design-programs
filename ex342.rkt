;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dir String -> [Maybe Path]
; produces a path to a file with name n; otherwise #false

(check-expect (find libs-dir "part1") #false)
(check-expect (find figure123 "part4") #false)
(check-expect (find figure123 "part1")
              '("TS" "Text" "part1"))
(check-expect (find figure123 "hang")
              '("TS" "Libs" "Code" "hang"))

(define (find d n)
  (local (; Path
          (define file-path
            (foldr (lambda (x y)
                     (if (string=? (file-name x) n)
                         `(,n)
                         y))
                   #false
                   (dir-files d)))

          ; Path
          (define dir-path
            (if (false? file-path)
                (foldr (lambda (x y)
                         (if (false? y)
                             (find x n)
                             y))
                       #false
                       (dir-dirs d))
                file-path)))
    (if (false? dir-path)
        #false
        (cons (dir-name d) dir-path))))

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
  (map (lambda (p) (cons (dir-name d) p))
       (append
        (foldr append '()
               (map (lambda (d)
                      (find-all d n))
                    (dir-dirs d)))
        (foldr append '()
               (map (lambda (f)
                      (if (string=? (file-name f) n) `((,n)) '()))
                    (dir-files d))))))