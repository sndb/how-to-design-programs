;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex65) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; introduced names:
; constructor make-movie
; selectors movie-title, movie-producer, movie-year
; predicate movie?

(define-struct person [name hair eyes phone])
; introduced names:
; constructor make-person
; selectors person-name, person-hair, person-eyes, person-phone
; predicate person?

(define-struct pet [name number])
; introduced names:
; constructor make-pet
; selectors pet-name, pet-number
; predicate pet?

(define-struct CD [artist title price])
; introduced names:
; constructor make-CD
; selectors CD-artist, CD-title, CD-price
; predicate CD?

(define-struct sweater [material size producer])
; introduced names:
; constructor make-sweater
; selectors sweater-material, sweater-size, sweater-producer
; predicate sweater?