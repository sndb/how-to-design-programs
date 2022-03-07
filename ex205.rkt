;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex205) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LLists
(define list-tracks (read-itunes-as-lists ITUNES-LOCATION))

; LAssoc Examples
(define ex-lassoc1
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

(define ex-lassoc2
  (list
   (list "Track ID" 444)
   (list "Name" "Only Time")
   (list "Artist" "Enya")
   (list "Album" "A Day Without Rain")
   (list "Genre" "New Age")
   (list "Kind" "MPEG audio file")
   (list "Size" 4364035)
   (list "Total Time" 218096)
   (list "Track Number" 3)
   (list "Track Count" 11)
   (list "Year" 2000)
   (list "Date Modified" (create-date 2002 7 17 0 0 21))
   (list "Date Added" (create-date 2002 7 17 3 55 42))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 18)
   (list "Play Date" 3388484327)
   (list "Play Date UTC" (create-date 2011 5 17 17 38 47))
   (list "Sort Album" "Day Without Rain")
   (list "Persistent ID" "EBBE9171392FA34A")
   (list "Track Type" "File")
   (list
    "Location"
    "file://localhost/Users/matthias/Music/iTunes/iTunes%20Music/Enya/A%20Day%20Without%20Rain/03%20Only%20Time.mp3")
   (list "File Folder Count" 4)
   (list "Library Folder Count" 1)))

(define ex-lassoc3
  (list
   (list "Track ID" 486)
   (list "Name" "Possession")
   (list "Artist" "Sarah McLachlan")
   (list "Album" "Fumbling Towards Ecstasy")
   (list "Genre" "Rock")
   (list "Kind" "MPEG audio file")
   (list "Size" 5599708)
   (list "Total Time" 279875)
   (list "Track Number" 1)
   (list "Track Count" 12)
   (list "Year" 1993)
   (list "Date Modified" (create-date 2002 7 17 23 46 50))
   (list "Date Added" (create-date 2002 7 18 3 46 4))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 3)
   (list "Play Date" 3278077468)
   (list "Play Date UTC" (create-date 2007 11 16 22 4 28))
   (list "Persistent ID" "EBBE9171392FA374")
   (list "Track Type" "File")
   (list
    "Location"
    "file://localhost/Users/matthias/Music/iTunes/iTunes%20Music/Sarah%20McLachlan/Fumbling%20Towards%20Ecstasy/01%20Possession.mp3")
   (list "File Folder Count" 4)
   (list "Library Folder Count" 1)))

(define ex-llists1 (list ex-lassoc1 ex-lassoc2))
(define ex-llists2 (list ex-lassoc1 ex-lassoc2 ex-lassoc3))