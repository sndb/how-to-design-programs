;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex199) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Track Examples
(define ex-track1
  (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 ex-date1 20 ex-date2))
(define ex-track2
  (create-track "Possession" "Sarah McLachlan" "Fumbling Towards Ecstasy" 279875 1 ex-date3 3 ex-date4))

; LTracks Examples
(define ex-ltracks (list ex-track1 ex-track2))