;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex333) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define figure123
  (make-dir "TS"
            `(,(make-dir "Text"
                         '("part1" "part2" "part3"))
              "read!"
              ,(make-dir "Libs"
                         `(,(make-dir "Code" '("hang" "draw"))
                           ,(make-dir "Docs" '("read!")))))))

; Dir.v2 -> N
; determines how many files d contains

(check-expect (how-many (make-dir "a" '())) 0)
(check-expect (how-many figure123) 7)

(define (how-many d)
  (local (; Dir.v2 -> N
          (define (how-many-dir d)
            (how-many-lofd (dir-content d)))

          ; LOFD -> N
          (define (how-many-lofd l)
            (cond
              [(empty? l) 0]
              [(string? (first l))
               (+ (how-many-file (first l)) (how-many-lofd (rest l)))]
              [else
               (+ (how-many-dir (first l)) (how-many-lofd (rest l)))]))

          ; File.v2 -> N
          (define (how-many-file f) 1))
    (how-many-dir d)))