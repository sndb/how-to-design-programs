;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex362) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-CON-DEF "cannot find a constant definition")
(define ERROR-FUN-DEF "cannot find a function definition")
(define ERROR-PARSE "cannnot parse an expression")

; An S-expr is one of: 
; – Atom
; – SL

; An Atom is one of: 
; – Number
; – Symbol

; An SL is one of: 
; – '()
; – (cons S-expr SL)

(define ex-da-sl
  '((define close-to-pi 3.14)
    (define (area-of-circle r)
      (* close-to-pi (* r r)))
    (define (volume-of-10-cylinder r)
      (* 10 (area-of-circle r)))))

(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-expr is one of: 
; – Number
; – Symbol
; — (make-fun Symbol BSL-expr)
; – (make-add BSL-expr BSL-expr)
; – (make-mul BSL-expr BSL-expr)
; Examples:
(define x 5)
(define y 3)
(define expr-5 'x)
(define expr-8 (make-add 'x 3))
(define expr-7.5 (make-mul 1/2 (make-mul 'x 3)))
(define expr-34 (make-add (make-mul 'x 'x) (make-mul 'y 'y)))
(define k2 (make-fun 'k (make-add 1 1)))
(define 5k2 (make-mul 5 k2))
(define i5k2 (make-mul (make-fun 'i 5) k2))

(define-struct def [name param body])
(define-struct con [name value])
; A BSL-fun-def is a structure:
;   (make-def Symbol Symbol BSL-expr)
; A BSL-con-def is a structure:
;   (make-con Symbol BSL-expr)
; A BSL-def is one of:
; — BSL-fun-def
; — BSL-con-def
; Examples:
(define close-to-pi
  (make-con 'close-to-pi 3.14))
(define area-of-circle
  (make-def 'area-of-circle 'r
            (make-mul 'close-to-pi (make-mul 'r 'r))))
(define volume-of-10-cylinder
  (make-def 'volume-of-10-cylinder 'r
            (make-mul 10 (make-fun 'area-of-circle 'r))))

; A BSL-da-all is a [List-of BSL-def].
; Examples:
(define ex-da
  (list close-to-pi area-of-circle volume-of-10-cylinder))

; S-expr SL -> Number
; interpret ex using da as a definition area

(check-expect (interpreter '(* 2 close-to-pi) ex-da-sl)
              6.28)
(check-expect (interpreter '(+ close-to-pi
                               (area-of-circle 2))
                           ex-da-sl)
              15.7)
(check-expect (interpreter '(* (area-of-circle 1)
                               (volume-of-10-cylinder 1))
                           ex-da-sl)
              98.596)
(check-error (interpreter '(* close-to-pi no-con) ex-da-sl)
             ERROR-CON-DEF)
(check-error (interpreter '(* close-to-pi (no-def 10)) ex-da-sl)
             ERROR-FUN-DEF)

(define (interpreter ex da)
  (eval-all (parse-ex ex) (parse-da da)))

; BSL-expr BSL-da-all -> Number
; evaluates ex using da as a definition area

(check-expect (eval-all (make-mul 2 'close-to-pi)
                        ex-da)
              6.28)
(check-expect (eval-all (make-add 'close-to-pi
                                  (make-fun 'area-of-circle 2))
                        ex-da)
              15.7)
(check-expect (eval-all (make-mul (make-fun 'area-of-circle 1)
                                  (make-fun 'volume-of-10-cylinder 1))
                        ex-da)
              98.596)
(check-error (eval-all (make-mul 'close-to-pi 'no-con)
                       ex-da)
             ERROR-CON-DEF)
(check-error (eval-all (make-mul 'close-to-pi
                                 (make-fun 'no-def 10))
                       ex-da)
             ERROR-FUN-DEF)

(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (eval-all (con-value (lookup-con-def da ex)) da)]
    [(fun? ex)
     (local (; Number
             (define arg (eval-all (fun-arg ex) da))
             ; BSL-fun-def
             (define fn (lookup-fun-def da (fun-name ex)))
             ; BSL-expr
             (define body (subst (def-body fn)
                                 (def-param fn)
                                 arg)))
       (eval-all body da))]
    [(add? ex)
     (+ (eval-all (add-left ex) da)
        (eval-all (add-right ex) da))]
    [(mul? ex)
     (* (eval-all (mul-left ex) da)
        (eval-all (mul-right ex) da))]))

; BSL-da-all Symbol -> BSL-con-def
; finds a constant definition of x; error otherwise

(check-expect (lookup-con-def ex-da 'close-to-pi)
              close-to-pi)
(check-error (lookup-con-def ex-da 'area-of-circle)
             ERROR-CON-DEF)
(check-error (lookup-con-def ex-da 'close-to-tau)
             ERROR-CON-DEF)

(define (lookup-con-def da x)
  (cond
    [(empty? da) (error ERROR-CON-DEF)]
    [(cons? da)
     (if (and (con? (first da))
              (symbol=? x (con-name (first da))))
         (first da)
         (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> BSL-fun-def
; finds a function definition of f; error otherwise

(check-expect (lookup-fun-def ex-da 'area-of-circle)
              area-of-circle)
(check-expect (lookup-fun-def ex-da 'volume-of-10-cylinder)
              volume-of-10-cylinder)
(check-error (lookup-fun-def ex-da 'close-to-pi)
             ERROR-FUN-DEF)
(check-error (lookup-fun-def ex-da 'area-of-square)
             ERROR-FUN-DEF)

(define (lookup-fun-def da f)
  (cond
    [(empty? da) (error ERROR-FUN-DEF)]
    [(cons? da)
     (if (and (def? (first da))
              (symbol=? f (def-name (first da))))
         (first da)
         (lookup-fun-def (rest da) f))]))

; BSL-expr Symbol Number -> BSL-expr
; replace all occurrences of x by v

(check-expect (subst expr-5 'x 3) 3)
(check-expect (subst expr-8 'x 10) (make-add 10 3))
(check-expect (subst expr-7.5 'x 7) (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst expr-34 'y 2) (make-add (make-mul 'x 'x)
                                             (make-mul 2 2)))
(check-expect (subst k2 'x 1) k2)

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(fun? ex) (make-fun (fun-name ex) (subst (fun-arg ex) x v))]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))

; S-expr -> BSL-expr

(check-expect (parse-ex 5) x)
(check-expect (parse-ex 'x) expr-5)
(check-expect (parse-ex '(+ x 3)) expr-8)
(check-expect (parse-ex '(+ (* x x) (* y y))) expr-34)
(check-expect (parse-ex '(* (i 5) (k (+ 1 1)))) i5k2)
(check-error (parse-ex '(/ 4 2)) ERROR-PARSE)
(check-error (parse-ex '(/ 8 4 2)) ERROR-PARSE)

(define (parse-ex s)
  (local (; S-expr -> BSL-expr
          (define (parse-sexp s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))

          ; Atom -> BSL-expr
          (define (parse-atom a)
            (cond
              [(number? a) a]
              [(symbol? a) a]))

          ; SL -> BSL-expr
          (define (parse-sl l)
            (cond
              [(and (= 2 (length l))
                    (symbol? (first l)))
               (make-fun (first l) (parse-ex (second l)))]
              [(and (= 3 (length l))
                    (symbol? (first l)))
               (cond
                 [(symbol=? (first l) '+)
                  (make-add (parse-ex (second l))
                            (parse-ex (third l)))]
                 [(symbol=? (first l) '*)
                  (make-mul (parse-ex (second l))
                            (parse-ex (third l)))]
                 [else (error ERROR-PARSE)])]
              [else (error ERROR-PARSE)])))
    (parse-sexp s)))

; SL -> BSL-da-all

(check-expect (parse-da ex-da-sl) ex-da)
(check-error (parse-da '(define (too many args) 0))
             ERROR-PARSE)
(check-error (parse-da '(define a b c))
             ERROR-PARSE)
(check-error (parse-da '(define (a b) c d))
             ERROR-PARSE)

(define (parse-da da)
  (local (; S-expr -> BSL-def
          (define (parse-def s)
            (cond
              [(and (list? s)
                    (= 3 (length s))
                    (symbol=? 'define (first s)))
               (cond
                 [(symbol? (second s))
                  (make-con (second s)
                            (parse-ex (third s)))]
                 [(and (list? (second s))
                       (= 2 (length (second s)))
                       (ormap symbol? (second s)))
                  (make-def (first (second s))
                            (second (second s))
                            (parse-ex (third s)))]
                 [else (error ERROR-PARSE)])]
              [else (error ERROR-PARSE)])))
    (map parse-def da)))
  
; Any -> Boolean
; is v atom
(define (atom? v)
  (or (number? v)
      (symbol? v)))