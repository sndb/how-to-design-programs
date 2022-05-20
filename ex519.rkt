;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex519) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Q: Is it acceptable to impose the extra cost on cons for all
; programs to turn length into a constant-time function?
; A: It depends on the context. For most programs, imposing the extra
; cost is unnecessary. For the programs or algorithms that depend on
; measuring the length of a list on each resursive step, it can turn
; their complexity from O(n2) to O(n).