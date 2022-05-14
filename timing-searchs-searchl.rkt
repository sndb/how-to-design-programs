;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname timing-searchs-searchl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number [List-of Number] -> Boolean
; is x in l
 
(check-expect (searchL 0 '(3 2 1 0)) #true)
(check-expect (searchL 4 '(3 2 1 0)) #false)
(check-expect (searchS 0 '(3 2 1 0)) #true)
(check-expect (searchS 4 '(3 2 1 0)) #false)

(define (searchL x l)
  (cond
    [(empty? l) #false]
    [else
     (or (= (first l) x)
         (searchL
          x (rest l)))]))

(define (searchS x l)
  (cond
    [(= (length l) 0) #false]
    [else
     (or (= (first l) x)
         (searchS
          x (rest l)))]))


; N -> [List Number Number]
; how long do searchS and searchL take 
; to look for n in (list 0 ... (- n 1))
(define (timing n)
  (local ((define long-list
            (build-list n (lambda (x) x))))
    (list
     (time (searchS n long-list))
     (time (searchL n long-list)))))