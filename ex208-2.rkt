;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex208-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

(define ex-lassoc
  (list
   (list "Track ID" 442)
   (list "Name" "Wild Child")
   (list "Artist" "Enya")
   (list "Album" "A Day Without Rain")
   (list "Genre" "New Age")
   (list "Kind" "MPEG audio file")
   (list "Size" 4562044)
   (list "Total Time" 227996)
   (list "Track Number" 2)
   (list "Track Count" 11)
   (list "Year" 2000)
   (list "Date Modified" (create-date 2002 7 17 0 0 11))
   (list "Date Added" (create-date 2002 7 17 3 55 14))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 20)
   (list "Play Date" 3388484113)
   (list "Play Date UTC" (create-date 2011 5 17 17 35 13))
   (list "Sort Album" "Day Without Rain")
   (list "Persistent ID" "EBBE9171392FA348")
   (list "Track Type" "File")
   (list
    "Location"
    "file://localhost/Users/matthias/Music/iTunes/iTunes%20Music/Enya/A%20Day%20Without%20Rain/02%20Wild%20Child.mp3")
   (list "File Folder Count" 4)
   (list "Library Folder Count" 1)))

(define ex-track
  (create-track "Wild Child"
                "Enya"
                "A Day Without Rain"
                227996
                2
                (create-date 2002 07 17 03 55 14)
                20
                (create-date 2011 05 17 17 35 13)))

; LAssoc -> Track
; converts LAssoc to a Track when possible

(check-error (track-as-struct '()))
(check-expect (track-as-struct ex-lassoc) ex-track)

(define (track-as-struct l)
  (create-track (find-association "Name" l "track-as-struct: name not found")
                (find-association "Artist" l "track-as-struct: artist not found")
                (find-association "Album" l "track-as-struct: album not found")
                (find-association "Total Time" l "track-as-struct: total time not found")
                (find-association "Track Number" l "track-as-struct: track number not found")
                (find-association "Date Added" l "track-as-struct: date added not found")
                (find-association "Play Count" l "track-as-struct: play count not found")
                (find-association "Play Date UTC" l "track-as-struct: play date utc not found")))

; String LAssoc String -> BSDN
; finds the second item of the first Association whose first item is equal to key, else error e

(check-error (find-association "Track ID" '() "err"))
(check-expect (find-association "Track ID" ex-lassoc "err") 442)

(define (find-association key l e)
  (if (not (false? (assoc key l)))
      (second (assoc key l))
      (error e)))