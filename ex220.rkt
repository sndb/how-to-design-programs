;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 15) ; # of blocks, vertically
(define WIDTH 10) ; # of blocks, horizontally
(define SIZE 10) ; blocks are squares
(define SCENE-HEIGHT (* HEIGHT SIZE))
(define SCENE-WIDTH (* WIDTH SIZE))


(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))


(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of:
; – '()
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; examples
(define block-dropping (make-block 0 0))
(define block-dropping-next (make-block 0 1))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define block-center (make-block (sub1 (/ WIDTH 2))
                                 (sub1 (/ HEIGHT 2))))

(define landscape0 '())
(define landscape1 (list block-landed))
(define landscape2 (list block-on-block block-landed))

(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-next (make-tetris block-dropping-next landscape0))
(define tetris1 (make-tetris block-dropping landscape1))
(define tetris2 (make-tetris block-dropping landscape2))


; Tetris -> Image
; turns t into an Image

(check-expect
 (tetris-render tetris0)
 (place-block (tetris-block tetris0) SCENE))

(check-expect
 (tetris-render tetris1)
 (place-block
  (tetris-block tetris1)
  (landscape-render (tetris-landscape tetris1))))

(check-expect
 (tetris-render tetris2)
 (place-block
  (tetris-block tetris2)
  (landscape-render (tetris-landscape tetris2))))

(define (tetris-render t)
  (place-block (tetris-block t)
               (landscape-render (tetris-landscape t))))


; Block Image -> Image
; places b at img

(check-expect
 (place-block block-center SCENE)
 (place-image BLOCK
              (+ (* (block-x block-center) SIZE) (/ SIZE 2))
              (+ (* (block-y block-center) SIZE) (/ SIZE 2))
              SCENE))

(define (place-block b img)
  (place-image BLOCK
               (+ (* (block-x b) SIZE) (/ SIZE 2))
               (+ (* (block-y b) SIZE) (/ SIZE 2))
               img))


; Landscape -> Image
; places the blocks the landscape constists of at SCENE

(check-expect (landscape-render landscape0) SCENE)

(check-expect
 (landscape-render landscape1)
 (place-block (first landscape1) SCENE))

(check-expect
 (landscape-render landscape2)
 (place-block (first landscape2)
              (place-block (second landscape2)
                           SCENE)))

(define (landscape-render l)
  (cond
    [(empty? l) SCENE]
    [(cons? l)
     (place-block (first l) (landscape-render (rest l)))]))