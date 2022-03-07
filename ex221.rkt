;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex221) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define next-block-dropping (make-block 1 0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define block-on-block-prev (make-block 0 (- HEIGHT 3)))
(define block-right-down (make-block (sub1 WIDTH) (sub1 HEIGHT)))
(define block-center-up (make-block (quotient WIDTH 2) 0))
(define block-center (make-block (quotient WIDTH 2)
                                 (quotient HEIGHT 2)))

(define landscape0 '())
(define landscape1 (list block-landed))
(define landscape2 (list block-on-block block-landed))

(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-next (make-tetris block-dropping-next landscape0))
(define tetris1 (make-tetris next-block-dropping landscape1))
(define tetris1-before (make-tetris block-landed landscape0))
(define tetris2 (make-tetris next-block-dropping landscape2))
(define tetris2-before (make-tetris block-on-block landscape1))


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


; Tetris -> Tetris
; drops the block down in a straight line, lands it on the
; floor or on blocks that are already resting

(check-expect (tetris-update tetris0) tetris0-next)
(check-expect (tetris-update tetris1-before) tetris1)
(check-expect (tetris-update tetris2-before) tetris2)

(define (tetris-update t)
  (cond
    [(block-landed? (tetris-block t) (tetris-landscape t))
     (make-tetris (block-next (tetris-block t))
                  (cons (tetris-block t)
                        (tetris-landscape t)))]
    [else
     (make-tetris (block-move (tetris-block t))
                  (tetris-landscape t))]))


; Block Landscape -> Boolean
; determines if b has landed at l

(check-expect (block-landed? block-dropping landscape0)
              #false)
(check-expect (block-landed? block-landed landscape0)
              #true)
(check-expect (block-landed? block-on-block-prev landscape1)
              #false)
(check-expect (block-landed? block-on-block landscape1)
              #true)

(define (block-landed? b l)
  (or (member? (block-move b) l)
      (= (block-y (block-move b)) HEIGHT)))


; Block -> Block
; creates another block at the top of the column to the
; right of the current one; wraps to the other side if necessary

(check-expect (block-next block-landed) next-block-dropping)
(check-expect (block-next block-right-down) block-dropping)

(define (block-next b)
  (make-block (modulo (add1 (block-x b)) WIDTH) 0))


; Block -> Block
; moves b 1 block down

(check-expect (block-move block-dropping) block-dropping-next)
(check-expect (block-move block-on-block-prev) block-on-block)

(define (block-move b)
  (make-block (block-x b) (add1 (block-y b))))


; Number -> Tetris
; starts the game with clock ticking at the rate r
(define (tetris-main r)
  (big-bang (make-tetris block-center-up '())
    [to-draw tetris-render]
    [on-tick tetris-update r]))