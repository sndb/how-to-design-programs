;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex80) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title director year])
(define (movie-function-template m)
  (... (movie-title m) ... (movie-director m) ... (movie-year m) ...))

(define-struct pet [name number])
(define (pet-function-template p)
  (... (pet-name p) ... (pet-number p) ...))

(define-struct CD [artist title price])
(define (CD-function-template cd)
  (... (CD-artist cd) ... (CD-title cd) ... (CD-price cd) ...))

(define-struct sweater [material size color])
(define (sweater-function-template s)
  (... (sweater-material s) ... (sweater-size s) ... (sweater-color s) ...))