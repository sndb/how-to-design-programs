;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex386) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; 
; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; interpretation: (make-data p d) represents a stock price p
; and its change d since the last time
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; StockWorld -> StockWorld
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld -> String] String -> Image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s

(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-error
 (get '(meta ((content "+1") (itemprop "F"))) "G")
 "not found")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

; Xexpr.v3 String -> [Maybe String]
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s; #false otherwise

(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-expect
 (get-xexpr '(head
              (meta ((content "+1") (itemprop "F"))))
            "F")
 "+1")
(check-expect
 (get-xexpr '(head
              (meta)
              (meta ((content "+1") (itemprop "F"))))
            "F")
 "+1")
(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "G")
 #false)
(check-expect (get-xexpr 'bye "F") #false)
(check-expect (get-xexpr "hello" "F") #false)
(check-expect (get-xexpr 42 "F") #false)

(define (get-xexpr x s)
  (cond
    [(symbol? x) #false]
    [(string? x) #false]
    [(number? x) #false]
    [(empty? (xexpr-attr x))
     (get-xexpr-list (xexpr-content x) s)]
    [else
     (if (and (symbol=? 'meta (xexpr-name x))
              (string=? s (find-attr (xexpr-attr x) 'itemprop)))
         (find-attr (xexpr-attr x) 'content)
         (get-xexpr-list (xexpr-content x) s))]))

; [List-of Xexpr.v3] String -> [Maybe String]
(define (get-xexpr-list x s)
  (cond
    [(empty? x) #false]
    [else
     (local ((define r (get-xexpr (first x) s)))
       (if (string? r)
           r
           (get-xexpr-list (rest x) s)))]))

; Xexpr -> Symbol
; retrieves the name of xe
(define (xexpr-name xe)
  (first xe))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
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
(define (find-attr la sy)
  (local ((define pair (assq sy la)))
    (if (false? pair)
        #false
        (second pair))))