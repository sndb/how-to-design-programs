;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex202) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

; Date Examples
(define ex-date1 (create-date 2002 07 17 03 55 14))
(define ex-date2 (create-date 2011 05 17 17 35 13))
(define ex-date3 (create-date 2002 07 18 03 46 04))
(define ex-date4 (create-date 2007 11 16 22 04 28))
(define ex-date5 (create-date 2002 07 17 03 55 42))
(define ex-date6 (create-date 2011 05 17 17 38 47))

; Track Examples
(define ex-track1
  (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 ex-date1 20 ex-date2))
(define ex-track2
  (create-track "Possession" "Sarah McLachlan" "Fumbling Towards Ecstasy" 279875 1 ex-date3 3 ex-date4))
(define ex-track3
  (create-track "Only Time" "Enya" "A Day Without Rain" 218096 3 ex-date5 18 ex-date6))

; LTracks Examples
(define ex-ltracks1 (list ex-track1 ex-track2))
(define ex-ltracks2 (list ex-track1 ex-track2 ex-track3))

; LTracks -> N
; produces the total amount of play time

(check-expect (total-time '()) 0)
(check-expect (total-time ex-ltracks1)
              (+ (track-time ex-track1) (track-time ex-track2)))

(define (total-time l)
  (cond
    [(empty? l) 0]
    [(cons? l)
     (+ (track-time (first l)) (total-time (rest l)))]))

; LTracks -> List-of-strings
; produces the list of album titles

(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles ex-ltracks1)
              (list "A Day Without Rain" "Fumbling Towards Ecstasy"))
(check-expect (select-all-album-titles ex-ltracks2)
              (list "A Day Without Rain" "Fumbling Towards Ecstasy" "A Day Without Rain"))

(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (track-album (first l)) (select-all-album-titles (rest l)))]))

; List-of-strings -> List-of-strings
; constructs a list that contains every String from l exactly once

(check-expect (create-set '()) '())
(check-expect (create-set (list "A Day Without Rain" "Fumbling Towards Ecstasy"))
              (list "A Day Without Rain" "Fumbling Towards Ecstasy"))
(check-expect (create-set (list "A Day Without Rain" "Fumbling Towards Ecstasy" "A Day Without Rain"))
              (list "A Day Without Rain" "Fumbling Towards Ecstasy"))

(define (create-set l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (first l) (remove-all (first l) (create-set (rest l))))]))

; LTracks -> List-of-strings
; produces a list of unique album titles

(check-expect (select-album-titles/unique '()) '())
(check-expect (select-album-titles/unique ex-ltracks1)
              (list "A Day Without Rain" "Fumbling Towards Ecstasy"))
(check-expect (select-album-titles/unique ex-ltracks2)
              (list "A Day Without Rain" "Fumbling Towards Ecstasy"))

(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))

; String LTracks -> LTracks
; extracts from l the list of tracks that belong to the album a

(check-expect (select-album "A Day Without Rain" '()) '())
(check-expect (select-album "A Day Without Rain" ex-ltracks1)
              (list ex-track1))
(check-expect (select-album "A Day Without Rain" ex-ltracks2)
              (list ex-track1 ex-track3))

(define (select-album a l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (if (string=? a (track-album (first l)))
         (cons (first l) (select-album a (rest l)))
         (select-album a (rest l)))]))