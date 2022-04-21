;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex410) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))

(define bad-db
  (make-db `(("Name" ,string?) ("Age" ,integer?))
           '(("Alice" 35) ("Bob" "25"))))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check bad-db) #false)

(define (integrity-check db)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define width   (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(second s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))

; DB [List-of Label] -> DB
; retains a column from db if its label is in labels

(check-expect
 (db-content (project school-db '("Name" "Present")))
 projected-content)

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

; DB [List-of Label] [Row -> Boolean] -> [List-of Row]
; produces a list of rows that satisfy p, projected down to labels

(check-expect (select school-db
                      '("Name" "Present")
                      (lambda (row) (> (second row) 30)))
              '(("Alice" #true)
                ("Dave" #false)))

(define (select db labels p)
  (db-content (project (make-db (db-schema db)
                                (filter p (db-content db)))
                       labels)))

; DB [List-of Label] -> DB
; reorders the columns according to lol

(check-expect
 (db-content (reorder presence-db '("Description" "Present")))
 '(("presence" #true)
   ("absence" #false)))
(check-expect
 (db-content (reorder presence-db '("Description" "Hello")))
 '(("presence")
   ("absence")))

(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          
          ; Label -> N
          (define (label-position label)
            (index (map first schema) label))

          ; [List-of N]
          (define positions
            (filter number? (map label-position lol)))
          
          ; Row -> Row
          (define (reorder-row row)
            (map (lambda (i) (list-ref row i)) positions)))
    (make-db (map (lambda (i) (list-ref schema i)) positions)
             (map reorder-row content))))

; DB DB -> DB
; joins two databases with the same schema
; eliminates duplicate rows

(check-expect
 (db-content (db-union school-db
                       (make-db school-schema
                                '(("Anna" 27 #false)
                                  ("John" 31 #true)
                                  ("Dave" 32 #false)))))
 '(("Anna"  27 #false)
   ("John"  31 #true)
   ("Alice" 35 #true)
   ("Bob"   25 #false)
   ("Carol" 30 #true)
   ("Dave"  32 #false)))

(define (db-union db1 db2)
  (make-db (db-schema db1)
           (foldr (lambda (row content)
                    (if (not (member? row content))
                        (cons row content)
                        content))
                  (db-content db1)
                  (db-content db2))))

; [X] [List-of X] X -> [Maybe N]
; produces the index of (first occurence of) x, #false otherwise

(check-expect (index '("a" "b" "c") "a") 0)
(check-expect (index '("a" "b" "c") "b") 1)
(check-expect (index '("a" "b" "c") "c") 2)
(check-expect (index '("a" "b" "c") "d") #false)

(define (index l x)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? x (first l))
         0
         (local ((define i (index (rest l) x)))
           (if (number? i)
               (add1 i)
               #false)))]))