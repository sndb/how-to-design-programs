;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex477) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
; termination: removes one item from w for each recursive step
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
     (foldr (lambda (item others)
              (local ((define without-item
                        (arrangements (remove item w)))
                      (define add-item-to-front
                        (map (lambda (a) (cons item a))
                             without-item)))
                (append add-item-to-front others)))
            '()
            w)]))
 
; [List-of [List-of 1String]] -> Boolean
; are the words "rat", "art", and "tar" members of the given list?
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)