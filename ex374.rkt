;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex374) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define SIZE 12) ; font size 
(define COLOR 'black) ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid COLOR) (text " " SIZE COLOR)))

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
;
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
;
; Examples:
(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; An XWord is '(word ((text String))).
;
; Examples:
(define w0 '(word ((text ""))))
(define w1 '(word ((text "hello"))))
(define w2 '(word ((text "hello world"))))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))
;
; Examples:
(define i1 '(li (word ((text "one")))))
(define i2 '(li (word ((text "two")))))
(define n0 `(ul ,i1 ,i2))
(define n0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; Image -> Image
; marks item with bullet 

(check-expect (bulletize (text "one" 12 'black))
              (beside/align 'center BT (text "one" 12 'black)))
(check-expect (bulletize n0-rendered)
              (beside/align 'center BT n0-rendered))

(define (bulletize i)
  (beside/align 'center BT i))

; XItem.v2 -> Image
; renders i

(check-expect (render-item i1)
              (bulletize (text "one" 12 'black)))
(check-expect (render-item i2)
              (bulletize (text "two" 12 'black)))
(check-expect (render-item `(li ,n0))
              (bulletize (render-enum n0)))

(define (render-item i)
  (local ((define content (first (xexpr-content i))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE COLOR)]
       [else
        (render-enum content)]))))

; XEnum.v2 -> Image
; renders e

(check-expect (render-enum n0) n0-rendered)

(define (render-enum e)
  (local (; XItem.v2 Image -> Image
          (define (draw i r)
            (above/align 'left (render-item i) r)))
    (foldr draw empty-image (xexpr-content e))))

; Any -> Boolean
; is x an Xexpr

(check-expect (xexpr? a0) #false)
(check-expect (xexpr? e0) #true)
(check-expect (xexpr? e1) #true)
(check-expect (xexpr? e2) #true)
(check-expect (xexpr? e3) #true)
(check-expect (xexpr? e4) #true)
(check-expect (xexpr? 0) #false)
(check-expect (xexpr? "hello") #false)
(check-expect (xexpr? '(123)) #false)

(define (xexpr? x)
  (local (; Any -> Boolean
          (define (body? x)
            (and (list? x)
                 (andmap xexpr? x)))
          ; Any -> Boolean
          (define (args? x)
            (and (list? x)
                 (andmap (lambda (p)
                           (and (list? p)
                                (= 2 (length p))
                                (symbol? (first p))
                                (string? (second p))))
                         x))))
    (and (cons? x)
         (symbol? (first x))
         (list? (rest x))
         (or (body? (rest x))
             (and (cons? (rest x))
                  (args? (first (rest x)))
                  (body? (rest (rest x))))))))

; Xexpr -> Symbol
; retrieves the name of xe

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)
(check-expect (xexpr-name '(action)) 'action)

(define (xexpr-name xe)
  (first xe))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; Xexpr -> Body
; retrieves the body of xe

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             (rest optional-loa+content)
             optional-loa+content))])))

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; [List-of Attribute] Symbol -> [Maybe String]
; retrieves the value associated with sy; otherwise #false

(check-expect (find-attr a0 'asdf) #false)
(check-expect (find-attr a0 'initial) "X")
(check-expect (find-attr '((a "a") (b "b") (c "c")) 'b) "b")
(check-expect (find-attr '((a "a") (b "b") (c "c")) 'c) "c")
(check-expect (find-attr '((a "a") (b "b") (c "c")) 'd) #false)

(define (find-attr la sy)
  (local ((define pair (assq sy la)))
    (if (false? pair)
        #false
        (second pair))))

; Any -> Boolean
; is x an XWord

(check-expect (word? "hello") #false)
(check-expect (word? 123) #false)
(check-expect (word? '(word ((txt "hello")))) #false)
(check-expect (word? '(wrd ((text "hello")))) #false)
(check-expect (word? w0) #true)
(check-expect (word? w1) #true)
(check-expect (word? w2) #true)

(define (word? x)
  (and (xexpr? x)
       (local ((define attrs (xexpr-attr x)))
         (and (symbol=? 'word (xexpr-name x))
              (empty? (xexpr-content x))
              (= 1 (length attrs))
              (string? (find-attr attrs 'text))))))

; XWord -> String
; extracts the text w contains

(check-expect (word-text w0) "")
(check-expect (word-text w1) "hello")
(check-expect (word-text w2) "hello world")

(define (word-text w)
  (find-attr (xexpr-attr w) 'text))