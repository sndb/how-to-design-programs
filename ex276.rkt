;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex276) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; Date Examples
(define ex-date1 (create-date 2002 07 17 03 55 14))
(define ex-date2 (create-date 2011 05 17 17 35 13))
(define ex-date3 (create-date 2002 07 18 03 46 04))
(define ex-date4 (create-date 2007 11 16 22 04 28))
(define ex-date5 (create-date 2002 07 17 03 55 42))
(define ex-date6 (create-date 2011 05 17 17 38 47))

; Track Examples
(define ex-track1
  (create-track "Wild Child"
                "Enya"
                "A Day Without Rain"
                227996
                2
                ex-date1
                20
                ex-date2))
(define ex-track2
  (create-track "Possession"
                "Sarah McLachlan"
                "Fumbling Towards Ecstasy"
                279875
                1
                ex-date3
                3
                ex-date4))
(define ex-track3
  (create-track "Only Time"
                "Enya"
                "A Day Without Rain"
                218096
                3
                ex-date5
                18
                ex-date6))

; LTracks Examples
(define ex-ltracks1 `(,ex-track1 ,ex-track2))
(define ex-ltracks2 `(,ex-track1 ,ex-track3 ,ex-track2))

; String Date LTracks -> LTracks
; extracts from l the list of tracks from the given album a
; that have been played after the date d

(check-expect
 (select-album-date "A Day Without Rain"
                    (create-date 2011 05 17 17 36 00)
                    '())
 '())
(check-expect
 (select-album-date "A Day Without Rain"
                    (create-date 2011 05 17 17 36 00)
                    ex-ltracks2)
 `(,ex-track3))
(check-expect
 (select-album-date "A Day Without Rain"
                    (create-date 2011 05 17 17 34 00)
                    ex-ltracks2)
 `(,ex-track1 ,ex-track3))

(define (select-album-date a d l)
  (local (; Track -> Boolean
          (define (f t)
            (and (string=? (track-album t) a)
                 (date>=? (track-played t) d))))
    (filter f l)))

; Date Date -> Boolean
; is d1 after d2?

(check-expect (date>=? (create-date 2011 05 17 17 34 00)
                       (create-date 2011 05 17 17 36 00))
              #false)
(check-expect (date>=? (create-date 2011 05 17 17 35 01)
                       (create-date 2011 05 17 17 35 00))
              #true)
(check-expect (date>=? (create-date 2011 05 17 16 36 10)
                       (create-date 2011 05 17 17 35 00))
              #false)
(check-expect (date>=? (create-date 2011 05 25 15 00 00)
                       (create-date 2011 05 17 17 35 00))
              #true)
(check-expect (date>=? (create-date 2011 04 18 18 40 00)
                       (create-date 2011 05 17 17 35 00))
              #false)
(check-expect (date>=? (create-date 2012 06 18 18 40 10)
                       (create-date 2011 05 17 17 35 00))
              #true)

(define (date>=? d1 d2)
  (or
   (> (date-year d1) (date-year d2))
   (and
    (= (date-year d1) (date-year d2))
    (or
     (> (date-month d1) (date-month d2))
     (and
      (= (date-month d1) (date-month d2))
      (or
       (> (date-day d1) (date-day d2))
       (and
        (= (date-day d1) (date-day d2))
        (or
         (> (date-hour d1) (date-hour d2))
         (and
          (= (date-hour d1) (date-hour d2))
          (or
           (> (date-minute d1) (date-minute d2))
           (and
            (= (date-minute d1) (date-minute d2))
            (>= (date-second d1) (date-second d2)))))))))))))

; LTracks -> [List-of LTracks]
; produces a list of LTracks, one per album

(check-expect (select-albums '())
              '())
(check-expect (select-albums ex-ltracks1)
              `((,ex-track1) (,ex-track2)))
(check-expect (select-albums ex-ltracks2)
              `((,ex-track1 ,ex-track3) (,ex-track2)))

(define (select-albums l)
  (local (; 1. select album titles
          (define titles (select-album-titles l))
          ; 2. select albums by titles
          (define albums (select-albums-by-titles l titles)))
    albums))

; LTracks -> [List-of String]
; produces a list of album titles in l

(check-expect (select-album-titles '())
              '())
(check-expect (select-album-titles ex-ltracks1)
              '("A Day Without Rain" "Fumbling Towards Ecstasy"))
(check-expect (select-album-titles ex-ltracks2)
              '("A Day Without Rain" "Fumbling Towards Ecstasy"))

(define (select-album-titles l)
  (local (; Track [List-of String] -> [List-of String]
          (define (add-album t y)
            (if (not (member? (track-album t) y))
                (cons (track-album t) y)
                y)))
    (foldr add-album '() l)))

; LTracks [List-of String] -> [List-of LTracks]
; produces a list of LTracks, one per album title

(check-expect (select-albums-by-titles ex-ltracks1
                                       '())
              '())
(check-expect (select-albums-by-titles ex-ltracks1
                                       '("A Day Without Rain"))
              `((,ex-track1)))
(check-expect (select-albums-by-titles ex-ltracks2
                                       '("A Day Without Rain"))
              `((,ex-track1 ,ex-track3)))
(check-expect (select-albums-by-titles ex-ltracks2
                                       '("A Day Without Rain"
                                         "Fumbling Towards Ecstasy"))
              `((,ex-track1 ,ex-track3) (,ex-track2)))

(define (select-albums-by-titles l titles)
  (local (; String -> LTracks
          (define (select-album a)
            (local (; Track -> Boolean
                    (define (f t)
                      (string=? a (track-album t))))
              (filter f l))))
    (map select-album titles)))