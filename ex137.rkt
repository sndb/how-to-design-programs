;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex137) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ...
      ... (contains-flatt? (rest alon)) ...)]))

(define (how-many alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (how-many (rest alos)) ...)]))