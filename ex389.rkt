;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex389) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combines the list of names and the list of phone numbers
; into the list of phone records
; assume the two lists are of equal length

(check-expect (zip '() '()) '())
(check-expect (zip '("A") '("737-1234"))
              (list (make-phone-record "A" "737-1234")))
(check-expect (zip '("A" "B") '("737-1234" "747-2345"))
              (list (make-phone-record "A" "737-1234")
                    (make-phone-record "B" "747-2345")))

(define (zip names numbers)
  [cond
    [(empty? names) '()]
    [else
     (cons (make-phone-record (first names) (first numbers))
           (zip (rest names) (rest numbers)))]])