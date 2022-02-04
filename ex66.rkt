;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex66) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
(define-struct person [name hair eyes phone])
(define-struct pet [name number])
(define-struct CD [artist title price])
(define-struct sweater [material size producer])

(make-movie "Reservoir Dogs" "Quentin Tarantino" 1992)
(make-person "Elizabeth" "black" "blue" "643-23-25")
(make-pet "cat" 2)
(make-CD "The Beatles" "Abbey Road" 4.99)
(make-sweater "cotton" "s" "Lacoste")