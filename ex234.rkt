;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex234) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/web-io)

; A RankedSong is a list:
;   '(N String).
; A List-of-ranked-songs is one of:
; — '()
; — (cons RankedSongs List-of-ranked-songs)

; List-of-strings -> List-of-ranked-songs
; adds ranks to los
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> List-of-ranked-songs
; adds ranks (in reverse order) to los
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(define one-list-ranked (ranking one-list))

; List-of-ranked-songs -> ... nested list ...
; produces a list representation of an HTML table

(check-expect
 (make-ranking one-list-ranked)
 `(table ((border "1"))
         (tr ,@(make-row '(1 "Asia: Heat of the Moment")))
         (tr ,@(make-row '(2 "U2: One")))
         (tr ,@(make-row '(3 "The White Stripes: Seven Nation Army")))))

(define (make-ranking lors)
  `(table ((border "1"))
          ,@(make-rows lors)))

; List-of-ranked-songs -> ... nested list ...

(check-expect
 (make-rows one-list-ranked)
 `((tr ,@(make-row '(1 "Asia: Heat of the Moment")))
   (tr ,@(make-row '(2 "U2: One")))
   (tr ,@(make-row '(3 "The White Stripes: Seven Nation Army")))))

(define (make-rows lors)
  (cond
    [(empty? lors) '()]
    [else
     (cons `(tr ,@(make-row (first lors)))
           (make-rows (rest lors)))]))

; Ranked-song -> ... nested list ...

(check-expect (make-row '(1 "Asia: Heat of the Moment"))
              '((td "1") (td "Asia: Heat of the Moment")))

(define (make-row rs)
  `((td ,(number->string (first rs))) (td ,(second rs))))