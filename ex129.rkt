;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex129) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define CELESTIAL-BODIES
  (cons "Mercury"
        (cons "Venus"
              (cons "Earth"
                    (cons "Mars"
                          (cons "Jupiter"
                                (cons "Saturn"
                                      (cons "Uranus"
                                            (cons "Neptune" '())))))))))

(define MEAL
  (cons "steak"
        (cons "french fries"
              (cons "beans"
                    (cons "bread"
                          (cons "water"
                                (cons "Brie cheese"
                                      (cons "ice cream" '()))))))))

(define COLORS
  (cons "red"
        (cons "orange"
              (cons "yellow"
                    (cons "green"
                          (cons "blue"
                                (cons "indigo"
                                      (cons "violet" '()))))))))