;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex130) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of:
; – '()
; – (cons String List-of-names)
; interpretation: a list of invitees, by last name

(cons "Smith"
      (cons "Johnson"
            (cons "Williams"
                  (cons "Brown"
                        (cons "Jones" '())))))