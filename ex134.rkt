;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex134) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of:
; – '()
; – (cons String List-of-names)
; interpretation: a list of contacts, by last name

; String List-of-names -> Boolean
; determines whether x occurs on l
(check-expect
 (contains? "Flatt" (cons "X" (cons "Y" (cons "Z" '()))))
 #false)
(check-expect
 (contains? "Flatt" (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(define (contains? x l)
  (cond
    [(empty? l) #false]
    [(cons? l)
     (or (string=? (first l) x)
         (contains? x (rest l)))]))

(contains? "Flatt"
           (cons "Fagan"
                 (cons "Findler"
                       (cons "Fisler"
                             (cons "Flanagan"
                                   (cons "Flatt"
                                         (cons "Felleisen"
                                               (cons "Friedman" '()))))))))