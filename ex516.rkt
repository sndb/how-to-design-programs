;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex516) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – Variable
; – Lambda
; – Application

(define-struct lmd (para body))
(define-struct app (fun arg))
; A Variable is a Symbol.
; A Lambda is a structure:
;   (make-lmd Variable Lam)
; An Application is a structure:
;   (make-app Lam Lam)

(define ex1 (make-lmd 'x 'x))
(define ex2 (make-lmd 'x 'y))
(define ex3 (make-lmd 'y (make-lmd 'x 'y)))
(define ex4 (make-app (make-lmd 'x (make-app 'x 'x))
                      (make-lmd 'x (make-app 'x 'x))))
(define ex5 (make-app (make-lmd 'x 'x) (make-lmd 'x 'x)))
(define ex6 (make-app (make-app (make-lmd 'y (make-lmd 'x 'y))
                                (make-lmd 'z 'z))
                      (make-lmd 'w 'w)))
(define ex7 'x)

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
 
(check-expect (undeclareds ex1)
              (make-lmd 'x '*declared:x))
(check-expect (undeclareds ex2)
              (make-lmd 'x '*undeclared:y))
(check-expect (undeclareds ex3)
              (make-lmd 'y (make-lmd 'x '*declared:y)))
(check-expect (undeclareds ex4)
              (make-app
               (make-lmd 'x (make-app '*declared:x '*declared:x))
               (make-lmd 'x (make-app '*declared:x '*declared:x))))
(check-expect (undeclareds (make-app (make-lmd 'x 'x)
                                     (make-lmd 'y 'x)))
              (make-app (make-lmd 'x '*declared:x)
                        (make-lmd 'y '*undeclared:x)))
(check-expect
 (undeclareds (make-lmd '*undeclared
                        (make-app
                         (make-lmd
                          'x (make-app 'x '*undeclared))
                         'y)))
 (make-lmd '*undeclared
           (make-app
            (make-lmd
             'x (make-app '*declared:x '*declared:*undeclared))
            '*undeclared:y)))

(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(symbol? le)
               (if (member? le declareds)
                   (string->symbol
                    (string-append "*declared:"
                                   (symbol->string le)))
                   (string->symbol
                    (string-append "*undeclared:"
                                   (symbol->string le))))]
              [(lmd? le)
               (local ((define para (lmd-para le))
                       (define body (lmd-body le))
                       (define newd (cons para declareds)))
                 (make-lmd para (undeclareds/a body newd)))]
              [(app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (make-app (undeclareds/a fun declareds)
                           (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))